#' settings_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_general_ui <- function(id){
  ns <- NS(id)

  collapsible_card(
    title = "General settings",
    checkboxInput_h(
      inputId = ns("theoretical"),
      label = "Theoretical calculations",
      value = FALSE
    ),
    checkboxInput_h(
      inputId = ns("fractional"),
      label = "Fractional values",
      value = FALSE
    )
  )
}

#' settings_general Server Functions
#'
#' @noRd
mod_settings_general_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      input_rv("theoretical", "fractional")
    )
  })
}
