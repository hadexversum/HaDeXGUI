#' settings_time UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_time_ui <- function(id, mode = "limits and points"){
  stopifnot(mode %in% c("limits and points", "only limits"))
  ns <- NS(id)

  switch(
    mode,
    `limits and points` = collapsible_card(
      title = "Timepoints",
      fluidRow(
        column(
          width = 6,
          checkboxGroupInput_h(
            inputId = ns("points"),
            label = "Show time points: ",
            choices = "",
            selected = ""
          )
        ),
        column(
          width = 6,
          wrap_div(
            selectInput_h(
              inputId = ns("0"),
              label = "Deut 0% Exposure",
              choices = "",
              selected = ""
            ),
            id = ns("0"),
            type = "visswitch"
          ),
          wrap_div(
            selectInput_h(
              inputId = ns("100"),
              label = "Deut 100% Exposure",
              choices = "",
              selected = ""
            ),
            id = ns("100"),
            type = "visswitch"
          )
        )
      )
    ),
    `only limits` = wrap_div(
      collapsible_card(
        title = "Timepoints",
        splitLayout(
          selectInput_h(
            inputId = ns("0"),
            label = "Deut 0% Exposure",
            choices = "",
            selected = ""
          ),
          wrap_div(
            selectInput_h(
              inputId = ns("100"),
              label = "Deut 100% Exposure",
              choices = "",
              selected = ""
            ),
            id = ns("100"),
            type = "visswitch"
          )
        )
      ),
      id = ns("0"),
      type = "visswitch"
    )
  )
}

#' settings_time Server Functions
#'
#' @noRd
mod_settings_time_server <- function(id,
                                     mode = "limits and points",
                                     p_times,
                                     p_times_with_control,
                                     p_no_deut_control,
                                     s_calculation) {
  stopifnot(mode %in% c("limits and points", "only limits"))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### reactive values from inputs

    time_0 <- reactive({
      if (s_calculation[["theoretical"]]())
        min(p_times()[p_times() > 0]) # TODO: should there be 0 instead?
      else
        as.numeric(input[["0"]])
    })

    time_100 <- reactive({
      if (s_calculation[["theoretical"]]() || !s_calculation[["fractional"]]())
        MAX_TIME # TODO: should there be max ()[] ... instead?
      else
        as.numeric(input[["100"]])
    })

    ### observers updating inputs

    observe({
      updateSelectInput(
        session,
        inputId = "0",
        choices = p_times()[p_times() < MAX_TIME],
        selected = p_times()[p_times() == p_no_deut_control()]
      )

      toggle_id(
        !s_calculation[["theoretical"]](),
        wrap_id(ns("0"), "visswitch")
      )
    })

    observe({
      wait_for(length(p_times_with_control()) > 1)
      wait_for(time_0())
      wait_for(time_100())

      bigger_than_0 <- p_times_with_control()[p_times_with_control() > time_0()]

      updateSelectInput(
        session,
        inputId = "100",
        choices = bigger_than_0,
        selected = max(bigger_than_0[bigger_than_0 < MAX_TIME])
      )

      toggle_id(
        !s_calculation[["theoretical"]]() && s_calculation[["fractional"]](),
        wrap_id(ns("100"), "visswitch")
      )
    })

    observe({
      wait_for(time_100())
      wait_for(time_0())

      times_t <- p_times()[p_times() > time_0() & p_times() < time_100()]

      updateCheckboxGroupInput(
        session,
        inputId = "points",
        choices = times_t,
        selected = times_t
      )
    })

    ### return values

    return(
      list(
        points = input_r_numeric("points"),
        `0` = time_0,
        `100` = time_100
      )
    )
  })
}

