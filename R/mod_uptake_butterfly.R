#' uptake_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_uptake_butterfly_ui <- function(id, differential) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = if (differential) "Butterfly Differential Plot" else "Butterfly Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      butterfly_general_settings(ns),
      butterfly_state(ns, differential),
      butterfly_timepoints(ns),
      butterfly_diff_test(ns) %nullify if% (!differential),
      butterfly_visualization(ns),

      # collapsed by default
      mod_zoom_ui(ns("zoom")),
      mod_settings_labels_ui(
        ns("labels"),
        label_prefix = if (differential) "Butterfly differential" else "Butterfly"
      )
    ),
    displayPanel = mod_plot_and_data_section_ui(
      ns("plot_and_data"),
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

butterfly_general_settings <- function(ns) collapsible_card(
  title = "General settings",
  checkboxInput_h(
    inputId = ns("theoretical"),
    label = "Theoretical calculations",
    value = FALSE
  ),
  checkboxInput_h(
    inputId = ns("fractional"),
    label = "Fractional values",
    value = FALSE
  )
)

butterfly_state <- function(ns, differential) {
  if (differential)
    collapsible_card(
      title = "States",
      p("Differential plot presents the uptake difference between State 1 and State 2."),
      splitLayout(
        selectInput_h(
          inputId = ns("state_1"),
          label = "State 1",
          choices = ""
        ),
        selectInput_h(
          inputId = ns("state_2"),
          label = "State 2",
          choices = ""
        )
      )
    )
  else
    collapsible_card(
      title = "State",
      selectInput_h(
        inputId = ns("state"),
        label = "Choose state:",
        choices = "",
        selected = ""
      )
    )
}

butterfly_timepoints <- function(ns) collapsible_card(
  title = "Timepoints",
  fluidRow(
    column(
      width = 6,
      checkboxGroupInput_h(
        inputId = ns("timepoints"),
        label = "Show time points: ",
        choices = "",
        selected = ""
      )
    ),
    column(
      width = 6,
      wrap_div(
        selectInput_h(
          inputId = ns("time_0"),
          label = "Deut 0%",
          choices = "",
          selected = ""
        ),
        id = ns("time_0"),
        type = "visswitch"
      ),
      wrap_div(
        selectInput_h(
          inputId = ns("time_100"),
          label = "Deut 100%",
          choices = "",
          selected = ""
        ),
        id = ns("time_100"),
        type = "visswitch"
      )
    )
  )
)

butterfly_diff_test <- function(ns) collapsible_card(
  title = "Test",
  fluidRow(
    column(
      width = 6,
      checkboxInput_h(
        inputId = ns("show_houde"),
        label = "Houde test",
        value = FALSE
      ),
      checkboxInput_h(
        inputId = ns("show_tstud"),
        label = "t-Student test",
        value = FALSE
      )
    ),
    column(
      width = 6,
      selectInput_h(
        inputId = ns("confidence_level"),
        label = "Select confidence level:",
        choices = c("80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
        selected = 0.98
      ),
      wrap_div(
        selectInput_h(
          inputId = ns("p_adjustment_method"),
          label = "Choose method of adjustment:",
          choices = c("none", "BH", "bonferroni"),
          selected = "none"
        ),
        id = ns("p_adjustment_method"),
        type = "visswitch"
      )
    )
  )
)

butterfly_visualization <- function(ns) collapsible_card(
  title = "Visualization",
  selectInput_h(
    inputId = ns("uncertainty"),
    label = "Show uncertainty as:",
    choices = c("ribbon", "bars", "bars + line"),
    selected = "ribbon"
  )
)

#' uptake_butterfly Server Functions
#'
#' @import ggplot2
#' @noRd
mod_uptake_butterfly_server <- function(
    id, differential,
    dat,
    chosen_protein, states_chosen_protein, times_from_file, times_with_control,
    deut_part, no_deut_control){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dat_processed <- if (differential) reactive({
      # TODO: check which validates are really needed
      validate(need(input[["state_1"]] != input[["state_2"]],
                    "There is no difference between the same state, choose two distinct states."))
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["state_1"]] %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["timepoints"]],
                    "Wait for parameters to be loaded"))

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state_1 = input[["state_1"]],
        state_2 = input[["state_2"]],
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        deut_part = deut_part() / 100
      ) %>%
        filter(Exposure %in% input[["timepoints"]])
    }) else reactive({
      # TODO: check which validates are really needed
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["state"]] %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["timepoints"]],
                    "Wait for parameters to be loaded"))

      HaDeX::create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = input[["state"]],
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        deut_part = deut_part() / 100
      ) %>%
        filter(Exposure %in% input[["timepoints"]])
    })

    plot_out <- if (differential) reactive({
      dat() %>%
        HaDeX::create_p_diff_uptake_dataset(
          diff_uptake_dat = dat_processed(),
          protein = chosen_protein(),
          state_1 = input[["state_1"]],
          state_2 = input[["state_2"]],
          confidence_level = as.numeric(input[["confidence_level"]]),
          p_adjustment_method = input[["p_adjustment_method"]],
          time_0 = as.numeric(input[["time_0"]]),
          time_100 = as.numeric(input[["time_100"]]),
          deut_part = deut_part() / 100
        ) %>%
        HaDeX::plot_differential_butterfly(
          diff_uptake_dat = dat_processed(),
          diff_p_uptake_dat = .,
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]],
          uncertainty_type = input[["uncertainty"]],
          show_houde_interval = input[["show_houde"]],
          show_tstud_confidence = input[["show_tstud"]],
          confidence_level = as.numeric(input[["confidence_level"]])
        )
    }) else reactive({
      validate(need(input[["timepoints"]],
                    "Wait for parameters to be loaded"))

      (dat_processed() %>%
        HaDeX::plot_butterfly(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]],
          uncertainty_type = input[["uncertainty"]]
        ) + geom_point_interactive(
          aes(tooltip = paste0(
            Sequence,
            "<br/>Position: ", Start, "-", End,
            "<br/>Value: ", round(value, 2),
            "<br/>Exposure: ", Exposure
          ))
        )
      ) %>% update_axes_and_labels(zoom, labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]]
        ) %>%
        filter(ID >= zoom[["x_range"]]()[[1]] &
               ID <= zoom[["x_range"]]()[[2]])
    })

    if (differential) {
      observe({
        updateSelectInput(
          session,
          inputId = "state_1",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[1]
        )
      })

      observe({
        updateSelectInput(
          session,
          inputId = "state_2",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[2]
        )
      })
    } else {
      observe({
        updateSelectInput(
          session,
          inputId = "state",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[1]
        )
      })
    }

    observe({
      updateSelectInput(
        session,
        inputId = "time_0",
        choices = times_from_file()[times_from_file() < MAX_TIME],
        selected = times_from_file()[times_from_file() == no_deut_control()]
      )

      toggle_id(
        !input[["theoretical"]],
        wrap_id(ns("time_0"), "visswitch")
      )
    })

    observe({
      updateSelectInput(
        session,
        inputId = "time_100",
        choices = times_with_control(),
        selected = max(times_with_control()[times_with_control() < MAX_TIME])
      )

      toggle_id(
        !input[["theoretical"]] && input[["fractional"]],
        wrap_id(ns("time_100"), "visswitch")
      )
    })

    if (differential) {
      observe({
        toggle_id(
          input[["show_tstud"]],
          wrap_id(ns("p_adjustment_method"), "visswitch")
        )
      })
    }

    observe({
      vec <- if (input[["fractional"]])
        times_from_file() < as.numeric(input[["time_100"]])
      else
        times_from_file() < MAX_TIME

      times_t <- times_from_file()[times_from_file() > input[["time_0"]] & vec]

      updateCheckboxGroupInput(
        session,
        inputId = "timepoints",
        choices = times_t,
        selected = times_t
      )
    })

    zoom =  mod_zoom_server(
      id = "zoom",
      dat_processed = dat_processed,
      fractional = input_r("fractional")
    )

    labels = mod_settings_labels_server(
      id = "labels",
      chosen_protein = chosen_protein,
      state = input_r("state"),
      theoretical = input_r("theoretical"),
      fractional = input_r("fractional")
    )

    mod_plot_and_data_section_server("plot_and_data", plot_out, dat_out)
  })
}
