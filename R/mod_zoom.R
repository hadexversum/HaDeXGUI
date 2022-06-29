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
mod_zoom_server <- function(id, dat_processed, fractional){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      validate(need(dat_processed(), "Wait for data to be processed"))
      max_x <- max(dat_processed()[["ID"]])
      min_x <- min(dat_processed()[["ID"]])

      updateSliderInput(
        session,
        inputId = "x_range",
        min = min_x,
        max = max_x,
        value = c(min_x, max_x),
      )
    })

    observe({
      validate(need(dat_processed(), "Wait for data to be processed"))
      if (fractional()){
        max_y <- ceiling(max(dat_processed()[["frac_deut_uptake"]], dat_processed()[["theo_frac_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_processed()[["frac_deut_uptake"]], dat_processed()[["theo_frac_deut_uptake"]], na.rm = TRUE)) - 1
      } else {
        max_y <- ceiling(max(dat_processed()[["deut_uptake"]], dat_processed()[["theo_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_processed()[["deut_uptake"]], dat_processed()[["theo_deut_uptake"]], na.rm = TRUE)) - 1
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
      input_rv("y_range", "x_range")
    )
  })
}
