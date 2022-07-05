#' plot_uptake UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_uptake_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' plot_uptake Server Functions
#'
#' @noRd 
mod_plot_uptake_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_plot_uptake_ui("plot_uptake_1")
    
## To be copied in the server
# mod_plot_uptake_server("plot_uptake_1")
