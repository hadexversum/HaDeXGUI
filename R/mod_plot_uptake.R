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
        names = c(
          "calculation", "state", "time", "peptide", "visualization", "test", "range", "label"
        ),
        modes = c(
          time = "only limits",
          state = if (differential) "double" else "disabled",
          test = if (differential) "selectible" else "disabled",
          peptide = if (differential) "single peptide" else "multiple peptides and states",
          visualization = "uptake"
        ),
        params = list(
          range_ids = c("y"),
          plot_type = "Uptake curves",
          differential = differential
        ),
        ns = ns
      )
    ),
    displayPanel = mod_display_plot_ui(
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

    peptide_table <- if (differential) reactive({
      dat() %>%
        filter(Protein == params[["chosen_protein"]]()) %>%
        select(Sequence, Start, End) %>%
        unique(.) %>%
        arrange(Start, End)
    }) else reactive({
      dat() %>%
        filter(Protein == params[["chosen_protein"]]()) %>%
        select(Sequence, State, Start, End) %>%
        unique(.) %>%
        arrange(Start, End)
    })

    dat_processed <- if (differential) reactive({
      validate(need(s_peptide[["selected"]](),
                    "Please select one peptide from the table on the left."))

      HaDeX::create_p_diff_uptake_dataset(
        dat = dat(),
        protein = params[["chosen_protein"]](),
        state_1 = s_state[["state_1"]](),
        state_2 = s_state[["state_2"]](),
        p_adjustment_method = s_test[["p_adjustment_method"]](),
        confidence_level = s_test[["confidence_level"]](),
        time_0 = s_time[["0"]](),
        time_100 = s_time[["100"]](),
        deut_part = params[["deut_part"]]()
      )
    }) else reactive({
      validate(need(s_peptide[["selected"]](),
                    "Please select at least one peptide from the table on the left."))
      validate(need(sum(s_time[["0"]]() < params[["times"]]() &
                          params[["times"]]() < s_time[["100"]]()),
                    "Not enough time points between Deut 0% and Deut 100%"))

      HaDeX::create_kinetic_dataset(
        dat = dat(),
        peptide_list = peptide_table()[s_peptide[["selected"]](), ],
        protein = params[["chosen_protein"]](),
        deut_part = params[["deut_part"]](),
        time_0 = s_time[["0"]](),
        time_100 = s_time[["100"]]()
      )
    })

    plot_out <- if (differential) reactive({
      HaDeX::plot_differential_uptake_curve(
        diff_p_uptake_dat = dat_processed(),
        sequence = peptide_table()[s_peptide[["selected"]](), "Sequence"],
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]](),
        uncertainty_type = s_visualization[["uncertainty_type"]](),
        log_x = s_visualization[["log_x"]](),
        show_houde_interval = s_test[["show_houde"]](),
        show_tstud_confidence = s_test[["show_tstud"]]()
      ) %>%
        update_axes_and_labels(range_y = s_range[["y"]], labels = s_label) %>%
        suppressMessages()
    }) else reactive({
      HaDeX::plot_kinetics(
        kin_dat = dat_processed(),
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]](),
        uncertainty_type = s_visualization[["uncertainty_type"]](),
        log_x = s_visualization[["log_x"]]()
      ) %>%
        update_axes_and_labels(range_y = s_range[["y"]], labels = s_label) %>%
        suppressMessages()
    })

    dat_out <- if (differential) reactive({
      HaDeX::show_diff_uptake_data(
        diff_uptake_dat = dat_processed(),
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]]()
      )
    }) else reactive({
      HaDeX::show_kinetic_data(
        kin_dat = dat_processed(),
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]]()
      )
    })

    range_specs <- list(
      if (differential) range_spec({
        if (s_calculation[["fractional"]]()) {
          max_abs <- round_any(max(dat_processed()[c("diff_frac_deut_uptake", "diff_theo_frac_deut_uptake")], na.rm = TRUE), 5, ceiling)

          list(
            min = -20,
            max = max_abs + 20,
            value = c(0, max_abs),
            step = 5
          )
        } else {
          max_abs <- round_any(
            max(dat_processed()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 1, ceiling
          )

          list(
            min = -5,
            max = max_abs + 5,
            value = c(0, max_abs)
          )
        }
      }, "y") else range_spec({
        if (s_calculation[["fractional"]]()) {
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
      names = c(
        "calculation", "state", "time", "peptide", "visualization", "test", "range", "label"
      ),
      modes = c(
        time = "only limits",
        state = if (differential) "double" else "single",
        test = if (differential) "selectible" else "disabled",
        peptide = if (differential) "single peptide" else "multiple peptides and states",
        visualization = "uptake"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)
  })
}
