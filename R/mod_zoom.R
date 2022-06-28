#' zoom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_zoom_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' zoom Server Functions
#'
#' @noRd 
mod_zoom_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_zoom_ui("zoom_1")
    
## To be copied in the server
# mod_zoom_server("zoom_1")
