#' settings_labels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_labels_ui <- function(id, plot_type, differential){
  ns <- NS(id)

  collapsible_card(
    title = "Adjust labels",
    fluidRow(
      column(
        width = 10,
        textInput(
          inputId = ns("title"),
          label = paste0(construct_plot_label(plot_type, differential), " title:"),
          value = "" # updatable by observer
        ),
        textInput(
          inputId = ns("x_lab"),
          label = paste0(construct_plot_label(plot_type, differential)," axis x label:"),
          value = "Peptide ID"
        ),
        textInput(
          inputId = ns("y_lab"),
          label = paste0(construct_plot_label(plot_type, differential), " axis y label:"),
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
mod_settings_labels_server <- function(id, chosen_protein, default_title, default_lab_y){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateTextInput(
        session,
        inputId = "title",
        value = default_title()
      )

      updateTextInput(
        session,
        inputId = "y_lab",
        value = default_lab_y()
      )
    })

    return(
      input_r_list("title", "x_lab", "y_lab", "title_size", "x_lab_size", "y_lab_size")
    )
  })
}
