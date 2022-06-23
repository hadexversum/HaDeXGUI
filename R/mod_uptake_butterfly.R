#' uptake_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uptake_butterfly_ui <- function(id) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = "Butterfly Plot",
    tagList(),
    tagList()
  )
}

#' uptake_butterfly Server Functions
#'
#' @noRd
mod_uptake_butterfly_server <- function(
    id, dat, chosen_protein,
    states_chosen_protein, times_from_file, times_with_control){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_uptake_butterfly_ui("uptake_butterfly_1")

## To be copied in the server
# mod_uptake_butterfly_server("uptake_butterfly_1")
