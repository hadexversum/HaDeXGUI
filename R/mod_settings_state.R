#' settings_state UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_state_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' settings_state Server Functions
#'
#' @noRd 
mod_settings_state_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_settings_state_ui("settings_state_1")
    
## To be copied in the server
# mod_settings_state_server("settings_state_1")
