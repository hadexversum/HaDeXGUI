#' hdexaminer_adjustement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hdexaminer_adjustement_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' hdexaminer_adjustement Server Functions
#'
#' @noRd 
mod_hdexaminer_adjustement_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_hdexaminer_adjustement_ui("hdexaminer_adjustement_1")
    
## To be copied in the server
# mod_hdexaminer_adjustement_server("hdexaminer_adjustement_1")
