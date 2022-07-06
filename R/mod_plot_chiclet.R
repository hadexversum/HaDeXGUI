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
          uncertainty_switch = "binary",
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
      validate(need(s_state[["state_1"]]() != s_state[["state_2"]](),
                    "There is no difference between the same state, choose two distinct states."))
      validate(need(params[["chosen_protein"]]() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(s_state[["state_1"]]() %in% params[["states_chosen_protein"]](),
                    "Wait for the parameters to be loaded."))
      validate(need(s_timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state_1 = s_state[["state_1"]](),
        state_2 = s_state[["state_2"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]]() %||% max(dat()[["Exposure"]]),
        deut_part = params[["deut_part"]]() / 100
      ) %>%
        filter(Exposure %in% s_timepoints[["timepoints"]]())
    }) else reactive({
      validate(need(params[["chosen_protein"]]() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(s_state[["state"]]() %in% params[["states_chosen_protein"]](),
                    "Wait for the parameters to be loaded."))
      validate(need(s_timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      dt <- HaDeX::create_state_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state = s_state[["state"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]]() %||% max(dat()[["Exposure"]]),
        deut_part = params[["deut_part"]]() / 100
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
           time_100 = s_timepoints[["time_100"]]()  %||% max(dat()[["Exposure"]]),
           deut_part = params[["deut_part"]]() / 100
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

    default_title <- if (differential) reactive({
      paste0(
        if (s_general[["theoretical"]]()) "Theoreotical c" else "C",
        "hiclet differential plot between ",
        s_state[["state_1"]](), " and ", s_state[["state_2"]]()
      )
    }) else reactive({
      paste0(
        if (s_general[["theoretical"]]()) "Theoreotical c" else "C",
        "hiclet plot for ",
        s_state[["state"]](), " state for ", params[["chosen_protein"]]()
      )
    })

    default_lab_y <- reactive({ "Exposure [min]" })

    range_specs <- list(
      range_spec({
        validate(need(dat_processed(), "Wait for data to be processed"))

        c(min(dat_processed()[["ID"]]), max(dat_processed()[["ID"]]))
      }, id = "x")
    )

    ### settings servers

    if (differential) {
      s_diff_test <- mod_settings_diff_test_server(
        id = "diff_test"
      )
    }

    invoke_settings_servers(
      names = c(
        "general", "state", "timepoints", "visualization", "range", "labels"
      ),
      const_params = list(uncertainty_switch = "binary")
    )

    ### plot server

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
