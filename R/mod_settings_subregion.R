#' settings_subregion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_subregion_ui <- function(id) {
  ns <- NS(id)

  collapsible_card(
    title = "Subregion",

    sliderInput(
      inputId = ns("subregion"),
      label = "Choose subregion of the protein sequence:",
      min = 1,
      max = 2,
      value = c(1, 2),
      step = 1
    ),
    fancy_icon = "grip-lines"
  )
}

#' settings_subregion Server Functions
#'
#' @noRd
mod_settings_subregion_server <- function(id, p_max_range) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### observers updating inputs

    observe({
      updateSliderInput(
        session = session,
        inputId = "subregion",
        max = p_max_range(),
        value = c(1, p_max_range())
      )
    })

    ### return

    return(
      input_r_list("subregion")
    )
  })
}

