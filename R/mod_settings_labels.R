#' settings_labels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_labels_ui <- function(id){
  ns <- NS(id)

  collapsible_card(
    title = "Adjust labels",
    fluidRow(
      column(width = 10,
             uiOutput(ns("gen_plot_title")),
             textInput(inputId = ns("plot_x_label"),
                       label = "Butterfly plot axis x label:",
                       value = "Peptide ID"),
             uiOutput(ns("gen_plot_y_label"))),
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

    output[["gen_plot_title"]] <- renderUI({
      textInput(
        inputId = ns("plot_title"),
        label = "Butterfly plot title:",
        value = if (theoretical())
          paste0("Theoreotical butterfly plot for ", state(), " state for ", chosen_protein())
        else
          paste0("Butterfly plot for ", state(), " state for ", chosen_protein())
      )
    })

    output[["gen_plot_y_label"]] <- renderUI({
      textInput(
        inputId = ns("plot_y_label"),
        label = "Butterfly plot axis y label:",
        value = if (fractional())
          "Fractional deuterium uptake [%]"
        else
          "Deuterium uptake [Da]"
      )
    })


    return(
      list(
        plot_title = reactive({ input[["plot_title"]] }),
        plot_x_label = reactive({ input[["plot_x_label"]] }),
        plot_y_label = reactive({ input[["plot_y_label"]] }),
        plot_title_size = reactive({ input[["plot_title_size"]] }),
        plot_x_label_size = reactive({ input[["plot_x_label_size"]] }),
        plot_y_label_size = reactive({ input[["plot_y_label_size"]] })
      )
    )
  })
}

## To be copied in the UI
# mod_settings_labels_ui("settings_labels_1")

## To be copied in the server
# mod_settings_labels_server("settings_labels_1")
