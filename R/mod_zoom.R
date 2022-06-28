#' zoom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zoom_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Zoom",
    sliderInput(
      inputId = ns("x_range"),
      label = "Choose x range for butterfly plot:",
      min = 0,
      max = 1,
      value = c(0, 1),
      step = 1
    ),
    sliderInput(
      inputId = ns("y_range"),
      label = "Choose y range for butterfly plot:",
      min = 0,
      max = 1,
      value = c(0, 1),
      step = 1
    ),
    init_collapsed = TRUE
  )
}

#' zoom Server Functions
#'
#' @noRd
mod_zoom_server <- function(id, dat_plot, fractional){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      max_x <- max(dat_plot()[["ID"]])
      min_x <- min(dat_plot()[["ID"]])

      updateSliderInput(
        session,
        inputId = "x_range",
        min = min_x,
        max = max_x,
        value = c(min_x, max_x),
      )
    })

    observe({
      if (fractional()){
        max_y <- ceiling(max(dat_plot()[["frac_deut_uptake"]], dat_plot()[["theo_frac_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_plot()[["frac_deut_uptake"]], dat_plot()[["theo_frac_deut_uptake"]], na.rm = TRUE)) - 1
      } else {
        max_y <- ceiling(max(dat_plot()[["deut_uptake"]], dat_plot()[["theo_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_plot()[["deut_uptake"]], dat_plot()[["theo_deut_uptake"]], na.rm = TRUE)) - 1
      }

      updateSliderInput(
        session,
        inputId = "y_range",
        min = min_y,
        max = max_y,
        value = c(min_y, max_y),
      )
    })

    return(
      list(
        y_range = input_r("y_range"),
        x_range = input_r("x_range")
      )
    )
  })
}
