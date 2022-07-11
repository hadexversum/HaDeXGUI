#' settings_subregion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_subregion_ui <- function(id) {
  ns <- NS(id)

  collapsible_card(
    title = "Subregion",

    sliderInput(
      inputId = ns("subregion"),
      label = "Choose subregion of the protein sequence:",
      min = 0,
      max = 1,
      value = c(0, 1),
      step = 1
    )
  )
}

#' settings_subregion Server Functions
#'
#' @noRd
mod_settings_subregion_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_settings_subregion_ui("settings_subregion_1")

## To be copied in the server
# mod_settings_subregion_server("settings_subregion_1")
