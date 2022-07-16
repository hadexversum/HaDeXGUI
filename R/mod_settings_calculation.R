#' settings_calculation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_calculation_ui <- function(id, mode = "FRAC AND THEO"){
  stopifnot(mode %in% c("FRAC AND THEO", "ONLY FRAC"))
  ns <- NS(id)

  collapsible_card(
    title = "Calculation",
    checkboxInput_h(
      inputId = ns("theoretical"),
      label = "Theoretical calculations",
      value = FALSE
    ) %nullify if% !(mode == "FRAC AND THEO"),
    checkboxInput_h(
      inputId = ns("fractional"),
      label = "Fractional values",
      value = FALSE
    ),
    fancy_icon = "cogs"
  )
}

#' settings_calculation Server Functions
#'
#' @noRd
mod_settings_calculation_server <- function(id, mode = "FRAC AND THEO"){
  stopifnot(mode %in% c("FRAC AND THEO", "ONLY FRAC"))
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(
      c(
        list(fractional = input_r("fractional")),
        if (mode == "FRAC AND THEO") list(theoretical = input_r("theoretical"))
        else list(theoretical = reactive({ FALSE }))
      )
    )
  })
}
