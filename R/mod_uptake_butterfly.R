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
mod_uptake_butterfly_ui <- function(id) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = "Butterfly Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      butterfly_general_settings(ns),
      butterfly_state(ns),
      butterfly_timepoints(ns),
      butterfly_visualization(ns),
      mod_zoom_ui(ns("zoom")),
      mod_settings_labels_ui(ns("butterfly_labels"), label_prefix = "Butterfly")
    ),
    displayPanel = mod_plot_and_data_section_ui(
      ns("butterfly_plot_and_data"),
      plot_label = "Butterfly plot",
      "The empty values (e.q. `Frac DU`) means there was not sufficient data
       for this peptide. Abbreviations from the table: DU - deuterium uptake,
       Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
    )
  )
}

butterfly_general_settings <- function(ns) collapsible_card(
  title = "General settings",
  checkboxInput_h(inputId = ns("theoretical"),
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = ns("fractional"),
                  label = "Fractional values",
                  value = FALSE)
)

butterfly_state <- function(ns) collapsible_card(
  title = "State",
  uiOutput(ns("gen_state"))
)

butterfly_timepoints <- function(ns) collapsible_card(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           uiOutput(ns("gen_timepoints"))
    ),
    column(width = 6,
           uiOutput(ns("gen_time_0")),
           uiOutput(ns("gen_time_100"))
    )
  )
)

butterfly_visualization <- function(ns) collapsible_card(
  title = "Visualization",
  selectInput_h(inputId = ns("uncertainty"),
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

#' uptake_butterfly Server Functions
#'
#' @import ggplot2
#' @noRd
mod_uptake_butterfly_server <- function(
    id, dat,
    chosen_protein, states_chosen_protein, times_from_file, times_with_control,
    deut_part, no_deut_control){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dat_plot <- reactive({
      # TODO: check which validates are really needed
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["state"]] %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))

      HaDeX::create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = input[["state"]],
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        deut_part = deut_part() / 100
      )
    })

    plot_obj <- reactive({
      validate(need(input[["timepoints"]], "Wait for parameters to be loaded"))

      dat_plot() %>%
        filter(Exposure %in% input[["timepoints"]]) %>%
        HaDeX::plot_butterfly(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]],
          uncertainty_type = input[["uncertainty"]]
        ) + geom_point_interactive(
          aes(tooltip = paste0(
            Sequence,
            "<br/>Position: ", Start, "-", End,
            "<br/>Value: ", round(value, 2),
            "<br/> Exposure: ", Exposure
          ))
        )
    })

    plot_out <- reactive({

      plot_obj() +
        coord_cartesian(
          xlim = c(zoom[["x_range"]]()[[1]], zoom[["x_range"]]()[[2]]),
          ylim = c(zoom[["y_range"]]()[[1]], zoom[["y_range"]]()[[2]])) +
        labs(
          title = labels[["plot_title"]](),
          x = labels[["plot_x_label"]](),
          y = labels[["plot_y_label"]]()) +
        theme(
          plot.title = element_text(size = labels[["plot_title_size"]]()),
          axis.text.x = element_text(size = labels[["plot_x_label_size"]]()),
          axis.title.x = element_text(size = labels[["plot_x_label_size"]]()),
          axis.title.y = element_text(size = labels[["plot_y_label_size"]]()),
          axis.text.y = element_text(size = labels[["plot_y_label_size"]]()),
          legend.text = element_text(size = labels[["plot_x_label_size"]]()),
          legend.title = element_text(size = labels[["plot_x_label_size"]]())
        )
    })

    dat_out <- reactive({
      dat_plot() %>%
        HaDeX::show_uptake_data(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]]
        ) %>%
        filter(Exposure %in% input[["timepoints"]]) %>%
        filter(ID >= zoom[["x_range"]]()[[1]] &
               ID <= zoom[["x_range"]]()[[2]])
    })

    output[["gen_state"]] <- renderUI({
      selectInput_h(
        inputId = ns("state"),
        label = "Choose state:",
        choices = states_chosen_protein(),
        selected = states_chosen_protein()[1]
      )
    })

    output[["gen_time_0"]] <- renderUI({
      selectInput_h(
        inputId = ns("time_0"),
        label = "Deut 0%",
        choices = times_from_file()[times_from_file() < 99999],
        selected = times_from_file()[times_from_file() == no_deut_control()]
      ) %visible if% !input[["theoretical"]]
    })

    output[["gen_time_100"]] <- renderUI({
      selectInput_h(
        inputId = ns("time_100"),
        label = "Deut 100%",
        choices = times_with_control(),
        selected = max(times_with_control()[times_with_control() < 99999])
      ) %visible if% (!input[["theoretical"]] && input[["fractional"]])
    })

    output[["gen_timepoints"]] <- renderUI({
      vec <- if (input[["fractional"]])
        times_from_file() < as.numeric(input[["time_100"]])
      else
        times_from_file() < 99999

      times_t <- times_from_file()[times_from_file() > input[["time_0"]] & vec]

      checkboxGroupInput_h(
        inputId = ns("timepoints"),
        label = "Show time points: ",
        choices = times_t,
        selected = times_t
      )
    })

    zoom =  mod_zoom_server(
      id = "zoom",
      dat_plot = dat_plot,
      fractional = input_r("fractional")
    )

    labels = mod_settings_labels_server(
      id = "butterfly_labels",
      chosen_protein = chosen_protein,
      state = input_r("state"),
      theoretical = input_r("theoretical"),
      fractional = input_r("fractional")
    )

    mod_plot_and_data_section_server("butterfly_plot_and_data", plot_out, dat_out)
  })
}
