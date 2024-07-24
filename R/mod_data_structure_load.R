#' data_structure_load UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_structure_load_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' data_structure_load Server Functions
#'
#' @noRd 
mod_data_structure_load_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_data_structure_load_ui("data_structure_load_1")
    
## To be copied in the server
# mod_data_structure_load_server("data_structure_load_1")
