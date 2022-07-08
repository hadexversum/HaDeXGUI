#' plot_chiclet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_chiclet_ui <- function(id, differential){
  ns <- NS(id)

  HaDeX_plotTab(
    title = construct_plot_label("Chiclet", differential, capitalize = TRUE),

    settingsPanel = rlang::exec(
      .fn = HaDeX_plotSettingsPanel,

      !!!install_settings_ui(
        names = c("general", "state", "timepoints", "diff_test", "visualization", "range", "labels"),
        params = list(
          differential = differential,
          uncertainty_mode = "binary",
          range_ids = "x",
          plot_type = "Chiclet"
        ),
        ns = ns
      ),
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Chiclet", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential)
    )
  )
}

#' plot_chiclet Server Functions
#'
#' @noRd
mod_plot_chiclet_server <- function(id, differential, dat, params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### reactives for data processing

    dat_processed <- if (differential) reactive({
      # TODO: check which validates are really needed
      validate(need(s_timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state_1 = s_state[["state_1"]](),
        state_2 = s_state[["state_2"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]](),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_timepoints[["timepoints"]]())
    }) else reactive({
      validate(need(s_timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      dt <- HaDeX::create_state_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state = s_state[["state"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]](),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_timepoints[["timepoints"]]())
    })

    plot_out <- if (differential) reactive({
      (dat() %>%
         HaDeX::create_p_diff_uptake_dataset(
           diff_uptake_dat = dat_processed(),
           protein = params[["chosen_protein"]](),
           state_1 = s_state[["state_1"]](),
           state_2 = s_state[["state_2"]](),
           confidence_level = s_diff_test[["confidence_level"]](),
           p_adjustment_method = s_diff_test[["p_adjustment_method"]](),
           time_0 = s_timepoints[["time_0"]](),
           time_100 = s_timepoints[["time_100"]](),
           deut_part = params[["deut_part"]]()
         ) %>% HaDeX::plot_differential_chiclet(
           diff_uptake_dat = dat_processed(),
           diff_p_uptake_dat = .,
           theoretical = s_general[["theoretical"]](),
           fractional = s_general[["fractional"]](),
           show_uncertainty = s_visualization[["show_uncertainty"]](),
           show_houde_interval = s_diff_test[["show_houde"]](),
           show_tstud_confidence = s_diff_test[["show_tstud"]](),
           confidence_level = s_diff_test[["confidence_level"]]()
         )
      ) %>% update_axes_and_labels(range_x = s_range[["x"]], labels = s_labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    }) else reactive({
      (dat_processed() %>%
         HaDeX::plot_chiclet(
           theoretical = s_general[["theoretical"]](),
           fractional = s_general[["fractional"]](),
           show_uncertainty = s_visualization[["show_uncertainty"]]()
         )
      ) %>% update_axes_and_labels(s_range[["x"]], labels = s_labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = s_general[["theoretical"]](),
          fractional = s_general[["fractional"]]()
        ) %>%
        filter(ID >= s_range[["x"]]()[[1]] &
               ID <= s_range[["x"]]()[[2]])
    })

    ### reactives for settings servers

    range_specs <- list(
      range_spec({
        wait_for(nrow(dat_processed()) > 0)

        list(
          min = min(dat_processed()[["ID"]]),
          max = max(dat_processed()[["ID"]])
        )
      }, id = "x")
    )

    label_specs <- list(
      label_spec(react_construct_uptake_title("butterfly", differential), "title"),
      label_spec("Exposure [min]", "y"),
      label_spec("Peptide ID", "x")
    )

    ### settings servers

    invoke_settings_servers(
      names = c(
        "general", "state", "timepoints", "visualization",
        "range", "labels", "diff_test"
      ),
      const_params = list(
        uncertainty_mode = "binary",
        log_x_switch = FALSE
      )
    )

    ### plot server

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
