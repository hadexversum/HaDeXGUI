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
    title = if (differential) "Chiclet Differential Plot" else "Chiclet Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      mod_settings_general_ui(ns("general")),
      mod_settings_state_ui(ns("state"), differential),
      mod_settings_timepoints_ui(ns("timepoints")),
      mod_settings_diff_test_ui(ns("diff_test")) %nullify if% (!differential),
      chiclet_visualization(ns),

      # collapsed by default
      mod_settings_range_ui(ns("range"), range_ids = "x"),
      mod_settings_labels_ui(
        ns("labels"),
        label_prefix = if (differential) "Chiclet differential" else "Chiclet"
      )
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = if (differential) "Chiclet differential plot" else "Chiclet plot",
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

chiclet_visualization <- function(ns) collapsible_card(
  title = "Visualization",
  checkboxInput_h(
    inputId = ns("uncertainty"),
    label = "Show uncertainty",
    value = TRUE
  )
)

#' plot_chiclet Server Functions
#'
#' @noRd
mod_plot_chiclet_server <- function(
    id, differential,
    dat,
    chosen_protein, states_chosen_protein, times_from_file, times_with_control,
    deut_part, no_deut_control) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### reactives for data processing

    dat_processed <- if (differential) reactive({}) else reactive({
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(state[["state"]]() %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))
      validate(need(timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      dt <- HaDeX::create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = state[["state"]](),
        time_0 = timepoints[["time_0"]](),
        time_100 = timepoints[["time_100"]]() %||% max(dat()[["Exposure"]]),
        deut_part = deut_part() / 100
      ) %>%
        filter(Exposure %in% timepoints[["timepoints"]]())
    })

    plot_out <- if (differential) reactive({}) else reactive({
      (dat_processed() %>%
         HaDeX::plot_chiclet(
           theoretical = general[["theoretical"]](),
           fractional = general[["fractional"]](),
           show_uncertainty = input[["uncertainty"]]
         )
      ) %>% update_axes_and_labels(range[["x"]], labels = labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = general[["theoretical"]](),
          fractional = general[["fractional"]]()
        ) %>%
        filter(ID >= range[["x"]]()[[1]] &
               ID <= range[["x"]]()[[2]])
    })

    ### reactives for settings servers

    default_title <- if (differential) reactive({
      paste0(
        if (general[["theoretical"]]()) "Theoreotical c" else "C",
        "hiclet differential plot between ",
        state[["state_1"]](), " and ", state[["state_2"]]()
      )
    }) else reactive({
      paste0(
        if (general[["theoretical"]]()) "Theoreotical c" else "C",
        "hiclet plot for ",
        state[["state"]](), " state for ", chosen_protein()
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
      s_general = general
    )

    range <- mod_settings_range_server(
      id = "range",
      range_specs = range_specs
    )

    labels <- mod_settings_labels_server(
      id = "labels",
      default_title = default_title,
      default_lab_y = default_lab_y
    )

    ### plot server

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
