#' uptake_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uptake_butterfly_ui <- function(id) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = "Butterfly Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      butterfly_general_settings(ns),
      butterfly_state(ns),
      butterfly_timepoints(ns),
      butterfly_visualization(ns),
      butterfly_zoom(ns),
      butterfly_labels_adjustement(ns)
    ),
    displayPanel = HaDeX_plotDisplayPanel(
      butterfly_plot_panel(ns),
      butterfly_debug(ns)
    )
  )
}

butterfly_general_settings <- function(ns) HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = ns("theoretical"),
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = ns("fractional"),
                  label = "Fractional values",
                  value = FALSE)
)

butterfly_state <- function(ns) HaDeX_plotSettingsSection(
  title = "State",
  selectInput_h(inputId = ns("state"),
                label = "Choose state:",
                choices = c("CD160", "CD160_HVEM"),
                selected = "CD160"),
)

butterfly_timepoints <- function(ns) HaDeX_plotSettingsSection(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           checkboxGroupInput_h(inputId = ns("timepoints"),
                                label = "Show time points: ",
                                choices = c(0.167, 1, 5, 25, 120, 1440),
                                selected = c(0.167, 1, 5, 25, 120, 1440))
    ),
    column(width = 6,
           div(id = ns("time_0_part"),
               selectInput_h(inputId = ns("time_0"),
                             label = "Deut 0%",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 0.001)
           ),
           div(id = ns("time_100_part"),
               selectInput_h(inputId = ns("time_100"),
                             label = "Deut 100%",
                             choices = c(0, 0.001, 1, 5, 25, 1440),
                             selected = 1440)
           )
    )
  )
)

butterfly_visualization <- function(ns) HaDeX_plotSettingsSection(
  title = "Visualization",
  selectInput_h(inputId = ns("uncertainty"),
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

butterfly_zoom <- function(ns) HaDeX_plotSettingsSection(
  title = "Zoom",
  sliderInput(inputId = ns("x_range"),
              label = "Choose x range for butterfly plot:",
              min = 1,
              max = 41,
              value = c(1, 41),
              step = 1),
  sliderInput(inputId = ns("y_range"),
              label = "Choose y range for butterfly plot:",
              min = 0,
              max = 100,
              value = c(0, 100),
              step = 1)
)

butterfly_labels_adjustement <- function(ns) HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-butterfly-labels-adjusting-panel"
  ),

  HaDeX_collapsablePanel(
    id = "HaDeX-butterfly-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             textInput(inputId = ns("plot_title"),
                       label = "Butterfly plot title:",
                       value = ""),
             textInput(inputId = ns("plot_x_label"),
                       label = "Butterfly plot axis x label:",
                       value = "Peptide ID"),
             textInput(inputId = ns("plot_y_label"),
                       label = "Butterfly plot axis y label:",
                       value = "Deuterium uptake [Da]")),
      column(width = 2,
             numericInput_h(inputId = ns("plot_title_size"),
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = ns("plot_x_label_size"),
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = ns("plot_y_label_size"),
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label.
      The legend text size is the same as the x axis label.")
  )
)

butterfly_plot_panel <- function(ns) tabsetPanel(
  tabPanel("Butterfly plot",
           plotOutput_h(ns("plot"), hover = hoverOpts(ns("plot_hover"), delay = 10, delayType = "debounce")),
           downloadButton(ns("plot_download_button"),
                          "Save chart (.svg)")),
  tabPanel("Data",
           DT::dataTableOutput(ns("plot_data")),
           p(
             "The empty values (e.q. `Frac DU`) means there was not sufficient data for this peptide.",
             "Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)

butterfly_debug <- function(ns) uiOutput(ns("plot_debug"))

#' uptake_butterfly Server Functions
#'
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

      create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = input[["state"]],
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        deut_part = deut_part() / 100
      )
    })

  })
}

## To be copied in the UI
# mod_uptake_butterfly_ui("uptake_butterfly_1")

## To be copied in the server
# mod_uptake_butterfly_server("uptake_butterfly_1")
