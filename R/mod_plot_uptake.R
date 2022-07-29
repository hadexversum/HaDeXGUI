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

  hadex_tab_plot(
    title = construct_plot_label("Uptake Curves", differential),

    settings = install_settings_ui(
      names = c(
        "calculation", "state", "time", "peptide", "visualization", "test", "range", "label"
      ),
      modes = c(
        time = "ONLY LIMITS",
        state = if (differential) "DOUBLE" else "MULTIPLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        peptide = "SINGLE",
        visualization = "UPTAKE"
      ),
      params = list(
        range_labs = construct_auto_range_labs("Uptake Curves", axes = "y", differential = differential),
        label_labs = construct_auto_label_labs("Uptake Curves", differential = differential)
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Uptake curves", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential),
      additional_plot_info = !differential,
      additional_button_server = mod_download_uptake_ui(ns("download_uptake"))
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
        select(Sequence, Start, End) %>%
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
                    "Please select one peptide from the table on the left."))
      validate(need(sum((s_time %()% 0) < (params %()% times) &
                          (params %()% times) < (s_time %()% 100)),
                    "Not enough time points between Deut 0% and Deut 100%"))

      peptide_subset <- peptide_table()[s_peptide %()% selected, ]

      HaDeX::calculate_peptide_kinetics(
        dat       = dat(),
        protein   = params  %()% chosen_protein,
        sequence  = peptide_subset[["Sequence"]],
        states    = s_state %()% states,
        start     = peptide_subset[["Start"]],
        end       = peptide_subset[["End"]],
        deut_part = params  %()% deut_part,
        time_0    = s_time  %()% 0,
        time_100  = s_time  %()% 100
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
      HaDeX::plot_uptake_curve(
        uc_dat = dat_processed(),
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
      HaDeX::show_uptake_data(
        uc_dat = dat_processed(),
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]]()
      )
    })

    info_out <- reactive({
      data <- HaDeX::calculate_auc(dat_processed(), state = s_state %()% states)
      glue::glue_data(data, "{Sequence}-{State} AUC: {round(auc, 4)}")
    }) %.?!% differential

    range_specs <- list(
      y = if (differential) range_spec({
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
      }) else range_spec({
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
      })
    )

    label_specs <- list(
      title = label_spec(react_construct_uptake_title("uptake curve", differential, include_state = FALSE)),
      y = label_spec(react_construct_uptake_lab_y(differential)),
      x = label_spec("Time point [min]")
    )

    invoke_settings_servers(
      names = c(
        "calculation", "state", "time", "peptide", "visualization", "test", "range", "label"
      ),
      modes = c(
        time = "ONLY LIMITS",
        state = if (differential) "DOUBLE" else "MULTIPLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        peptide = "SINGLE",
        visualization = "UPTAKE"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out, info_out = info_out)

    mod_download_uptake_server(
      id = "download_uptake",
      dat, params, peptide_table, s_time, s_calculation, s_visualization
    )

    return(
      c(
        autoreturn(),
        list(report_validation_peptide_selected = s_peptide[["selected_flag"]])
      )
    )
  })
}
