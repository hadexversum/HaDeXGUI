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
          "general",
          if (differential) "state" else NULL,
          "timepoints", "peptide", "visualization", "diff_test", "range", "labels"
        ),
        params = list(
          differential = differential,
          uncertainty_mode = "select",
          timepoints_mode = "only deut",
          log_x_switch = TRUE,
          range_ids = c("y"),
          plot_type = "Uptake curves",
          peptide_mode = if (differential) "single peptide" else "peptide and state"
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
        p_adjustment_method = s_diff_test[["p_adjustment_method"]](),
        confidence_level = s_diff_test[["confidence_level"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]](),
        deut_part = params[["deut_part"]]()
      )
    }) else reactive({
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

    plot_out <- if (differential) reactive({
      HaDeX::plot_differential_uptake_curve(
        diff_p_uptake_dat = dat_processed(),
        sequence = peptide_table()[s_peptide[["selected"]](), "Sequence"],
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]](),
        uncertainty_type = s_visualization[["uncertainty_type"]](),
        log_x = s_visualization[["log_x"]](),
        show_houde_interval = s_diff_test[["show_houde"]](),
        show_tstud_confidence = s_diff_test[["show_tstud"]]()
      ) %>%
        update_axes_and_labels(range_y = s_range[["y"]], labels = s_labels) %>%
        suppressMessages()
    }) else reactive({
      HaDeX::plot_kinetics(
        kin_dat = dat_processed(),
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]](),
        uncertainty_type = s_visualization[["uncertainty_type"]](),
        log_x = s_visualization[["log_x"]]()
      ) %>%
        update_axes_and_labels(range_y = s_range[["y"]], labels = s_labels) %>%
        suppressMessages()
    })

    dat_out <- if (differential) reactive({
      HaDeX::show_diff_uptake_data(
        diff_uptake_dat = dat_processed(),
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]]()
      )
    }) else reactive({
      HaDeX::show_kinetic_data(
        kin_dat = dat_processed(),
        theoretical = s_general[["theoretical"]](),
        fractional = s_general[["fractional"]]()
      )
    })

    range_specs <- list(
      if (differential) range_spec({
        if (s_general[["fractional"]]()) {
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
      names = c(
        "general",
        if (differential) "state" else NULL,
        "timepoints", "peptide", "visualization", "diff_test", "range", "labels"
      ),
      const_params = list(
        # general:
        theoretical_switch = TRUE,
        # visualization:
        uncertainty_mode = "select",
        log_x_switch = TRUE,
        # peptide:
        peptide_mode = if (differential) "single peptide" else "peptide and state",
        # diff_test:
        test_mode = "select shown"
      )
    )

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
