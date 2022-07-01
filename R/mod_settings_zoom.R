#' zoom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_zoom_ui <- function(id, hide_x = FALSE, hide_y = FALSE){
  ns <- NS(id)
  collapsible_card(
    title = "Zoom",
    sliderInput(
      inputId = ns("x_range"),
      label = "Choose x range for plot:",
      min = 0,
      max = 1,
      value = c(0, 1),
      step = 1
    ) %nullify if% hide_x,
    sliderInput(
      inputId = ns("y_range"),
      label = "Choose y range for plot:",
      min = 0,
      max = 1,
      value = c(0, 1),
      step = 1
    ) %nullify if% hide_y,
    init_collapsed = TRUE
  )
}

#' zoom Server Functions
#'
#' @noRd
mod_settings_zoom_server <- function(id,
                                     full_range_x = NULL,
                                     full_range_y = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (!is.null(full_range_x)) {
      observe({
        updateSliderInput(
          session,
          inputId = "x_range",
          min = full_range_x()[1],
          max = full_range_x()[2],
          value = full_range_x(),
        )
      })
    }

    if (!is.null(full_range_y)) {
      observe({
        updateSliderInput(
          session,
          inputId = "y_range",
          min = full_range_y()[1],
          max = full_range_y()[2],
          value = full_range_y(),
        )
      })
    }

    return(
      if (is.null(full_range_x))
        list(y_range = input_r("y_range"))
      else if (is.null(full_range_y))
        list(x_range = input_r("x_range"))
      else
        input_r_list("x_range", "y_range")
    )
  })
}
