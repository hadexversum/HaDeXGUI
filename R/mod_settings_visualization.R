#' settings_visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_visualization_ui <- function(id, uncertainty_mode,
                                          log_x_switch = FALSE,
                                          volcano_switch = FALSE){
  ns <- NS(id)
  collapsible_card(
    title = "Visualization",

    checkboxInput_h(
      inputId = ns("log_x"),
      label = "Logaritmic x scale",
      value = TRUE
    ) %nullify if% !log_x_switch,
    switch(
      uncertainty_mode,
      binary = checkboxInput_h(
        inputId = ns("show_uncertainty"),
        label = "Show uncertainty",
        value = TRUE
      ),
      select = selectInput_h(
        inputId = ns("uncertainty_type"),
        label = "Show uncertainty as:",
        choices = c("ribbon", "bars", "bars + line"),
        selected = "ribbon"
      ),
      none = NULL
    ),
    tagList(
      selectInput_h(
        inputId = "shown_interval",
        label = "Show confidence limit for: ",
        choices = c("All time points", "Selected time points"),
        selected = "All time points"
      ),
      checkboxInput_h(
        inputId = "distinguish_timepoints",
        label = "Distinguish timepoints by color",
        value = TRUE
      ),
      checkboxInput_h(
        inputId = "hide_insignificant",
        label = "Hide insignificant values?",
        value = FALSE
      ),
      checkboxInput_h(
        inputId = "show_insignificant_grey",
        label = "Show insignificant values in grey?",
        value = FALSE
      )
    ) %nullify if% !volcano_switch
  )
}

#' settings_visualization Server Functions
#'
#' @noRd
mod_settings_visualization_server <- function(id, uncertainty_mode, log_x_switch, volcano_switch){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      c(
        if (uncertainty_mode == "binary") list(show_uncertainty = input_r("show_uncertainty")) else NULL,
        if (uncertainty_mode == "select") list(uncertainty_type = input_r("uncertainty_type")) else NULL,
        if (log_x_switch) list(log_x = input_r("log_x")) else NULL,
        if (volcano_switch) list(
          shown_interval = input_r("shown_interval"),
          distinguish_timepoints = input_r("distinguish_timepoints"),
          hide_insignificant = input_r("hide_insignificant"),
          show_insignificant_grey = input_r("show_insignificant_grey")
        )
      )
    )
  })
}
