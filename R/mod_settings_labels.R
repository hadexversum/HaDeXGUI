#' settings_labels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_labels_ui <- function(id, label_prefix){
  ns <- NS(id)

  collapsible_card(
    title = "Adjust labels",
    fluidRow(
      column(
        width = 10,
        textInput(
          inputId = ns("title"),
          label = paste0(label_prefix, " plot title:"),
          value = "" # updatable by observer
        ),
        textInput(
          inputId = ns("x_lab"),
          label = paste0(label_prefix, " plot axis x label:"),
          value = "Peptide ID"
        ),
        textInput(
          inputId = ns("y_lab"),
          label = paste0(label_prefix, " plot axis y label:"),
          value = "" # updatable by observer
        )
      ),
      column(
        width = 2,
        numericInput_h(
          inputId = ns("title_size"),
          label = "Size:",
          value = 15,
          min = 5
        ),
        numericInput_h(
          inputId = ns("x_lab_size"),
          label = "Size:",
          value = 15,
          min = 5
        ),
        numericInput_h(
          inputId = ns("y_lab_size"),
          label = "Size:",
          value = 15,
          min = 5
        )
      )
    ),
    p("The axis ticks have the same size as the axis label.
      The legend text size is the same as the x axis label."),
    init_collapsed = TRUE
  )
}

#' settings_labels Server Functions
#'
#' @noRd
mod_settings_labels_server <- function(id, chosen_protein, state, theoretical, fractional){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateTextInput(
        session,
        inputId = "title",
        value = ic( if (theoretical())
          paste0("Theoreotical butterfly plot for ", state(), " state for ", chosen_protein())
          else
            paste0("Butterfly plot for ", state(), " state for ", chosen_protein()))
      )

      updateTextInput(
        session,
        inputId = "y_lab",
        value = if (fractional())
          "Fractional deuterium uptake [%]"
        else
          "Deuterium uptake [Da]"
      )
    })

    return(
      list(
        title = input_r("title"),
        x_lab = input_r("x_lab"),
        y_lab = input_r("y_lab"),
        title_size = input_r("title_size"),
        x_lab_size = input_r("x_lab_size"),
        y_lab_size = input_r("y_lab_size")
      )
    )
  })
}

## To be copied in the UI
# mod_settings_labels_ui("settings_labels_1")

## To be copied in the server
# mod_settings_labels_server("settings_labels_1")
