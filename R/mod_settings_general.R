#' settings_calculation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_calculation_ui <- function(id, mode = "frac and theo"){
  stopifnot(mode %in% c("frac and theo", "only frac"))
  ns <- NS(id)

  collapsible_card(
    title = "Calculation",
    checkboxInput_h(
      inputId = ns("theoretical"),
      label = "Theoretical calculations",
      value = FALSE
    ) %nullify if% !(mode == "frac and theo"),
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
mod_settings_calculation_server <- function(id, mode = "frac and theo"){
  stopifnot(mode %in% c("frac and theo", "only frac"))
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(
      c(
        list(fractional = input_r("fractional")),
        if (mode == "frac and theo") list(theoretical = input_r("theoretical"))
        else list(theoretical = reactive({ FALSE }))
      )
    )
  })
}
