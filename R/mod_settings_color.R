#' settings_color UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_color_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Colors",
    fancy_icon = "palette"
  )
}
#' settings_color Server Functions
#'
#' @noRd
mod_settings_color_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_settings_color_ui("settings_color_1")

## To be copied in the server
# mod_settings_color_server("settings_color_1")
