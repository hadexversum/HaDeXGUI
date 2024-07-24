#' plot_structure_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_structure_heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' plot_structure_heatmap Server Functions
#'
#' @noRd 
mod_plot_structure_heatmap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_plot_structure_heatmap_ui("plot_structure_heatmap_1")
    
## To be copied in the server
# mod_plot_structure_heatmap_server("plot_structure_heatmap_1")
