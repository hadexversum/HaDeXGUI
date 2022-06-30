#' plot_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_plot_butterfly_ui <- function(id, differential) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = if (differential) "Butterfly Differential Plot" else "Butterfly Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      mod_settings_general_ui(ns("general")),
      mod_settings_state_ui(ns("state"), differential),
      mod_settings_timepoints_ui(ns("timepoints")),
      mod_settings_diff_test_ui(ns("diff_test")) %nullify if% (!differential),
      butterfly_visualization(ns),

      # collapsed by default
      mod_settings_zoom_ui(ns("zoom")),
      mod_settings_labels_ui(
        ns("labels"),
        label_prefix = if (differential) "Butterfly differential" else "Butterfly"
      )
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = if (differential) "Butterfly differential plot" else "Butterfly plot",
      additional_data_info = if (differential) {
        "The table presents data from the chosen x plot range.
        The empty values (e.q. `Frac Diff DU`) mean there was not sufficient
        data for this peptide. There is a possibility that the measurement
        result is available for only one state of the peptide.
        Abbreviations from the table: Diff DU - differential deuterium uptake,
        Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
      } else {
        "The table presents data from the chosen x plot range.
        The empty values (e.q. `Frac DU`) means there was not sufficient data
        for this peptide. Abbreviations from the table: DU - deuterium uptake,
        Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
      }
    )
  )
}


butterfly_visualization <- function(ns) collapsible_card(
  title = "Visualization",
  selectInput_h(
    inputId = ns("uncertainty"),
    label = "Show uncertainty as:",
    choices = c("ribbon", "bars", "bars + line"),
    selected = "ribbon"
  )
)

#' plot_butterfly Server Functions
#'
#' @import ggplot2
#' @noRd
mod_plot_butterfly_server <- function(
    id, differential,
    dat,
    chosen_protein, states_chosen_protein, times_from_file, times_with_control,
    deut_part, no_deut_control){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dat_processed <- if (differential) reactive({
      # TODO: check which validates are really needed
      validate(need(state[["state_1"]]() != state[["state_2"]](),
                    "There is no difference between the same state, choose two distinct states."))
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(state[["state_1"]]() %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))
      validate(need(timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state_1 = state[["state_1"]](),
        state_2 = state[["state_2"]](),
        time_0 = timepoints[["time_0"]](),
        time_100 = timepoints[["time_100"]](),
        deut_part = deut_part() / 100
      ) %>%
        filter(Exposure %in% timepoints[["timepoints"]]())
    }) else reactive({
      # TODO: check which validates are really needed
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(state[["state"]]() %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))
      validate(need(timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      HaDeX::create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = state[["state"]](),
        time_0 = timepoints[["time_0"]](),
        time_100 = timepoints[["time_100"]](),
        deut_part = deut_part() / 100
      ) %>%
        filter(Exposure %in% timepoints[["timepoints"]]())
    })

    plot_out <- if (differential) reactive({
      (dat() %>%
         HaDeX::create_p_diff_uptake_dataset(
           diff_uptake_dat = dat_processed(),
           protein = chosen_protein(),
           state_1 = state[["state_1"]](),
           state_2 = state[["state_2"]](),
           confidence_level = diff_test[["confidence_level"]](),
           p_adjustment_method = diff_test[["p_adjustment_method"]](),
           time_0 = timepoints[["time_0"]](),
           time_100 = timepoints[["time_100"]](),
           deut_part = deut_part() / 100
         ) %>% HaDeX::plot_differential_butterfly(
           diff_uptake_dat = dat_processed(),
           diff_p_uptake_dat = .,
           theoretical = general[["theoretical"]](),
           fractional = general[["fractional"]](),
           uncertainty_type = input[["uncertainty"]],
           show_houde_interval = diff_test[["show_houde"]](),
           show_tstud_confidence = diff_test[["show_tstud"]](),
           confidence_level = diff_test[["confidence_level"]]()
         ) +
         geom_point_interactive(
           data = dat_processed(),
           aes(x = ID, y = 10, tooltip = paste0( #PLACEHOLDER
             Sequence,
             "<br/>Position: ", Start, "-", End,
             "<br/>Value: PLACEHOLDER", #TODO: find a way of obtaining this value seamlessly
             "<br/>Exposure: ", Exposure, " min"
           ))
         )
      ) %>% update_axes_and_labels(zoom, labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    }) else reactive({
      validate(need(timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      (dat_processed() %>%
        HaDeX::plot_butterfly(
          theoretical = general[["theoretical"]](),
          fractional = general[["fractional"]](),
          uncertainty_type = input[["uncertainty"]]
        ) +
          geom_point_interactive( #TODO: fix this redundancy?
            aes(tooltip = paste0(
              Sequence,
              "<br/>Position: ", Start, "-", End,
              "<br/>Value: ", round(value, 2),
              "<br/>Exposure: ", Exposure, " min"
            ))
          )
      ) %>% update_axes_and_labels(zoom, labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = general[["theoretical"]](),
          fractional = general[["fractional"]]()
        ) %>%
        filter(ID >= zoom[["x_range"]]()[[1]] &
               ID <= zoom[["x_range"]]()[[2]])
    })

    if (differential) {
      diff_test <- mod_settings_diff_test_server(
        id = "diff_test"
      )
    }

    general <- mod_settings_general_server(id = "general")

    state <- mod_settings_state_server(
      id = "state",
      differential = differential,
      states_chosen_protein = states_chosen_protein
    )

    timepoints <- mod_settings_timepoints_server(
      id = "timepoints",
      times_from_file = times_from_file,
      times_with_control = times_with_control,
      no_deut_control = no_deut_control,
      settings_general = general
    )

    zoom <- mod_settings_zoom_server(
      id = "zoom",
      dat_processed = dat_processed,
      fractional = general[["fractional"]],
      differential = differential
    )

    default_title <- if (differential) reactive({
      paste0(
        if (general[["theoretical"]]()) "Theoreotical b" else "B",
        "utterfly differential plot between ",
        state[["state_1"]](), " and ", state[["state_2"]]()
      )
    }) else reactive({
      paste0(
        if (general[["theoretical"]]()) "Theoreotical b" else "B",
        "utterfly plot for ",
        state[["state"]](), " state for ", chosen_protein()
      )
    })

    default_lab_y <- if (differential) reactive({
      if (general[["fractional"]]()) "Fractional deuterium uptake difference [%]"
      else "Deuterium uptake difference [Da]"
    }) else reactive({
      if (general[["fractional"]]()) "Fractional deuterium uptake [%]"
      else "Deuterium uptake [Da]"
    })

    labels <- mod_settings_labels_server(
      id = "labels",
      default_title = default_title,
      default_lab_y = default_lab_y
    )

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
