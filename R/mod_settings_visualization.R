#' settings_visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_visualization_ui <- function(id, uncertainty_switch, log_x_switch = FALSE){
  ns <- NS(id)
  collapsible_card(
    title = "Visualization",

    checkboxInput_h(
      inputId = ns("log_x"),
      label = "Logaritmic x scale",
      value = TRUE
    ) %nullify if% !log_x_switch,
    switch(
      uncertainty_switch,
      binary = checkboxInput_h(
        inputId = ns("show_uncertainty"),
        label = "Show uncertainty",
        value = TRUE
      ),
      select = selectInput_h(
        inputId = ns("uncertainty_mode"),
        label = "Show uncertainty as:",
        choices = c("ribbon", "bars", "bars + line"),
        selected = "ribbon"
      )
    )
  )
}

#' settings_visualization Server Functions
#'
#' @noRd
mod_settings_visualization_server <- function(id, uncertainty_switch){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      c(
        if (uncertainty_switch == "binary") list(show_uncertainty = input_r("show_uncertainty")) else NULL,
        if (uncertainty_switch == "select") list(uncertainty_mode = input_r("uncertainty_mode")) else NULL
      )
    )
  })
}
