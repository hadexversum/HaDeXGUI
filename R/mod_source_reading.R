#' source_reading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_source_reading_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' source_reading Server Functions
#'
#' @noRd 
mod_source_reading_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_source_reading_ui("source_reading_1")
    
## To be copied in the server
# mod_source_reading_server("source_reading_1")
