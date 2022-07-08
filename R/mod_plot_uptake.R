#' plot_uptake UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_uptake_ui <- function(id, differential) {
  ns <- NS(id)

  HaDeX_plotTab(
    title = construct_plot_label("Uptake Curves", differential, capitalize = TRUE),

    settingsPanel = rlang::exec(
      .fn = HaDeX_plotSettingsPanel,

      !!!install_settings_ui(
        names = c("general", "timepoints", "peptide",
                  "visualization", "range", "labels"),
        params = list(
          differential = differential,
          uncertainty_switch = "select",
          timepoints_switch = "only deut",
          log_x_switch = TRUE,
          range_ids = c("y"),
          plot_type = "Uptake curves"
        ),
        ns = ns
      )
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Uptake curves", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential)
    )
  )
}

#' plot_uptake Server Functions
#'
#' @noRd
mod_plot_uptake_server <- function(id, differential, dat, params){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    peptide_table <- reactive({
      dat() %>%
        filter(Protein == params[["chosen_protein"]]()) %>%
        select(Sequence, State, Start, End) %>%
        unique(.) %>%
        arrange(Start, End)
    })

    dat_processed <- reactive({
      # browser()
      validate(need(s_peptide[["selected"]](),
                    "Please select at least one peptide from the table on the left."))
      validate(need(sum(s_timepoints[["time_0"]]() < params[["times"]]() &
                          params[["times"]]() < s_timepoints[["time_100"]]()),
                    "Not enough time points between Deut 0% and Deut 100%"))

      HaDeX::create_kinetic_dataset(
        dat = dat(),
        peptide_list = peptide_table()[s_peptide[["selected"]](), ],
        protein = params[["chosen_protein"]](),
        deut_part = params[["deut_part"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]]()
      )
    })

    plot_out <- reactive({
      HaDeX::plot_kinetics(
        kin_dat = dat_processed(),
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]](),
        uncertainty_type = s_visualization[["uncertainty_mode"]](),
        log_x = s_visualization[["log_x"]]()
      ) %>% update_axes_and_labels(range_y = s_range[["y"]], labels = s_labels) %>%
        suppressMessages()
    })

    dat_out <- reactive({
      HaDeX::show_kinetic_data(
        kin_dat = dat_processed(),
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]]()
      )
    })

    range_specs <- list(
      range_spec({
        if (s_general[["fractional"]]()) {
          list(
            min = -50,
            max = 200,
            value = c(-10, 100),
            step = 10
          )
        } else {
          max_abs <- round_any(max(dat_processed()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, ceiling)

          list(
            min = -5,
            max = max_abs + 5,
            value = c(0, max_abs)
          )
        }
      }, id = "y")
    )

    label_specs <- list(
      label_spec(react_construct_uptake_title("uptake curve", differential, include_state = FALSE), "title"),
      label_spec(react_construct_uptake_lab_y(differential), "y"),
      label_spec("Time point [min]", "x")
    )

    invoke_settings_servers(
      c("general", "timepoints", "peptide",
        "visualization", "range", "labels"),
      const_params = list(
        uncertainty_switch = "select",
        log_x_switch = TRUE
      )
    )

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}

