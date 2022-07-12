#' plot_comparison_and_woods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_comparison_and_woods_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' plot_comparison_and_woods Server Functions
#'
#' @noRd 
mod_plot_comparison_and_woods_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_plot_comparison_and_woods_ui("plot_comparison_and_woods_1")
    
## To be copied in the server
# mod_plot_comparison_and_woods_server("plot_comparison_and_woods_1")
