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
  stopifnot(mode %in% c("limits and points", "only limits", "limits and exposure"))
  ns <- NS(id)

  deut_0 <- selectInput_h(
    inputId = ns("0"),
    label = "Deut 0% Exposure",
    choices = "",
    selected = ""
  )

  deut_100 <- selectInput_h(
    inputId = ns("100"),
    label = "Deut 100% Exposure",
    choices = "",
    selected = ""
  )

  timepoints <- checkboxGroupInput_h(
    inputId = ns("points"),
    label = "Show time points: ",
    choices = "",
    selected = ""
  )

  card_timepoints <- function(...) collapsible_card(
    title = "Timepoints",
    ...,
    fancy_icon = "stopwatch"
  )

  switch(
    mode,
    `limits and points` = card_timepoints(
      fluidRow(
        column(
          width = 6,
          toggleable(deut_0, id = ns("0")),
          toggleable(deut_100, id = ns("100"))
        ),
        column(
          width = 6,
          timepoints
        )
      )
    ),
    `only limits` = toggleable(
      card_timepoints(
        splitLayout(
          deut_0,
          toggleable(
            deut_100,
            id = ns("100")
          )
        )
      ),
      id = ns("0")
    ),
    `limits and exposure` = card_timepoints(
      fluidRow(
        column(
          width = 6,
          toggleable(deut_0, id = ns("0")),
          toggleable(deut_100, id = ns("100")),
          checkboxInput_h(
            inputId = ns("multiple_exposures"),
            label = "Use multiple Exposures?",
            value = FALSE
          )
        ),
        column(
          width = 6,
          toggleable(
            selectInput_h(
              inputId = ns("t"),
              label = "Measurement Exposure",
              choices = "",
              selected = ""
            ),
            id = ns("t")
          ),
          toggleable(timepoints, id = ns("points"))
        )
      )
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
  stopifnot(mode %in% c("limits and points", "only limits", "limits and exposure"))
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
        id = ns("0")
      )
    })

    observe({
      wait_for(length(p_times_with_control()) > 1)
      wait_for(time_0())

      bigger_than_0 <- p_times_with_control()[p_times_with_control() > time_0()]

      updateSelectInput(
        session,
        inputId = "100",
        choices = bigger_than_0,
        selected = max(bigger_than_0[bigger_than_0 < MAX_TIME])
      )

      toggle_id(
        !s_calculation[["theoretical"]]() && s_calculation[["fractional"]](),
        id  = ns("100")
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

