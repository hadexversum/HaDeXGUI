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
mod_zoom_server <- function(id, dat_processed, fractional, differential){
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

    y_var_theo <- reactive({
      construct_var_name(
        "diff" %nullify if% (!differential),
        "theo",
        "frac" %nullify if% (!fractional()),
        "deut_uptake"
      )
    })

    y_var_ntheo <- reactive({
      construct_var_name(
        "diff" %nullify if% (!differential),
        "frac" %nullify if% (!fractional()),
        "deut_uptake"
      )
    })

    observe({
      validate(need(dat_processed(), "Wait for data to be processed"))

      theo <- dat_processed()[[
        construct_var_name(
          differential,
          TRUE,
          fractional(),
          "deut_uptake"
        )
      ]]
      ntheo <- dat_processed()[[
        construct_var_name(
          differential,
          FALSE,
          fractional(),
          "deut_uptake"
        )
      ]]

      max_y <- ceiling(max(theo, ntheo, na.rm = TRUE)) + 1
      min_y <- floor(min(theo, ntheo, na.rm = TRUE)) - 1

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
