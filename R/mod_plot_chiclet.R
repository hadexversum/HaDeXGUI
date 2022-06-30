#' plot_chiclet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_chiclet_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' plot_chiclet Server Functions
#'
#' @noRd 
mod_plot_chiclet_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_plot_chiclet_ui("plot_chiclet_1")
    
## To be copied in the server
# mod_plot_chiclet_server("plot_chiclet_1")
