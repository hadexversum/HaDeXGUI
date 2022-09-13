#' settings_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_variable_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Variable",
    selectizeInput_h(
      inputId = ns("variable"),
      label = "Select variable to map for color:",
      choices = c(
        "AUC", "Back-exchange",
        "Deuterium uptake", "Uncertainty(Deuterium uptake)",
        "Differential deuterium uptake", "Uncertainty(Differential deuterium uptake)"
      ),
      selected = "AUC"
    ),
    fancy_icon = "table"
  )
}

#' settings_variable Server Functions
#'
#' @noRd
mod_settings_variable_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      input_r_list("variable")
    )
  })
}
