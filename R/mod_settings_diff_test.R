#' settings_diff_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_diff_test_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' settings_diff_test Server Functions
#'
#' @noRd 
mod_settings_diff_test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_settings_diff_test_ui("settings_diff_test_1")
    
## To be copied in the server
# mod_settings_diff_test_server("settings_diff_test_1")
