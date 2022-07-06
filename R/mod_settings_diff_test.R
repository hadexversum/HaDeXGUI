#' settings_diff_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_diff_test_ui <- function(id, differential){
  ns <- NS(id)

  collapsible_card(
    title = "Test",
    fluidRow(
      column(
        width = 6,
        checkboxInput_h(
          inputId = ns("show_houde"),
          label = "Houde test",
          value = FALSE
        ),
        checkboxInput_h(
          inputId = ns("show_tstud"),
          label = "t-Student test",
          value = FALSE
        )
      ),
      column(
        width = 6,
        selectInput_h(
          inputId = ns("confidence_level"),
          label = "Select confidence level:",
          choices = c("80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999),
          selected = 0.98
        ),
        wrap_div(
          selectInput_h(
            inputId = ns("p_adjustment_method"),
            label = "Choose method of adjustment:",
            choices = c("none", "BH", "bonferroni"),
            selected = "none"
          ),
          id = ns("p_adjustment_method"),
          type = "visswitch"
        )
      )
    )
  ) %nullify if% !differential
}

#' settings_diff_test Server Functions
#'
#' @noRd
mod_settings_diff_test_server <- function(id, differential){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### observers modifying input

    observe({
      toggle_id(
        input[["show_tstud"]],
        wrap_id(ns("p_adjustment_method"), "visswitch")
      )
    })

    ### return values

    return(
      list(
        show_houde = input_r("show_houde"),
        show_tstud = input_r("show_tstud"),
        confidence_level = input_r_numeric("confidence_level"),
        p_adjustment_method = input_r("p_adjustment_method")
      )
    )
  }) %nullify if% !differential
}

