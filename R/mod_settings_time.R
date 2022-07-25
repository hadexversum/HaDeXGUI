#' settings_time UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_time_ui <- function(id, mode = "LIMITS AND POINTS"){
  stopifnot(mode %in% c("LIMITS AND POINTS", "ONLY LIMITS", "LIMITS AND EXPOSURE",
                        "SINGLE POINT", "LIMITS AND SINGLE EXPOSURE"))
  ns <- NS(id)

  deut_0 <- selectizeInput_h(
    inputId = ns("0"),
    label = "Deut 0% Exposure:"
  )

  deut_100 <- selectizeInput_h(
    inputId = ns("100"),
    label = "Deut 100% Exposure:"
  )

  timepoints <- checkboxGroupInput_h(
    inputId = ns("points"),
    label = "Show time points:"
  )

  exposure <- selectizeInput_h( #TODO: fix this cropping issue also for other selects
    inputId = ns("t"),
    label = "Measurement Exposure:"
  )

  card_timepoints <- function(...) collapsible_card(
    title = "Timepoints",
    ...,
    fancy_icon = "stopwatch"
  )

  switch(
    mode,
    `LIMITS AND POINTS` = card_timepoints(
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
    `ONLY LIMITS` = toggleable(
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
    `LIMITS AND EXPOSURE` = card_timepoints(
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
          toggleable(exposure, id = ns("t")),
          toggleable(timepoints, id = ns("points"))
        )
      )
    ),
    `LIMITS AND SINGLE EXPOSURE` = card_timepoints(
      splitLayout(deut_0, exposure)
    ),
    `SINGLE POINT` = card_timepoints(exposure)
  )
}

#' settings_time Server Functions
#'
#' @noRd
mod_settings_time_server <- function(id,
                                     mode = "LIMITS AND POINTS",
                                     p_times,
                                     p_times_with_control,
                                     p_no_deut_control,
                                     s_calculation) {
  stopifnot(mode %in% c("LIMITS AND POINTS", "ONLY LIMITS", "LIMITS AND EXPOSURE",
                        "SINGLE POINT", "LIMITS AND SINGLE EXPOSURE"))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (mode == "SINGLE POINT") {
      observe({
        values <- p_times()[p_times() < MAX_TIME]

        updateSelectInput(
          session,
          inputId = "t",
          choices = values,
          selected = middle(values)
        )
      })
    } else {
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

      times_t <- reactive({
        wait_for(time_100())
        wait_for(time_0())

        p_times()[p_times() > time_0() & p_times() < time_100()]
      })

      observe({
        updateCheckboxGroupInput(
          session,
          inputId = "points",
          choices = times_t(),
          selected = times_t()
        )
      })

      ### limits-and-exposure-mode-specific observers

      if (mode %in% c("LIMITS AND EXPOSURE", "LIMITS AND SINGLE EXPOSURE")) {
        observe({
          validate(need(length(times_t) > 0,
                        "There should be at least one valid option between Exposure 0% and 100% to choose from."))

          updateSelectInput(
            session,
            inputId = "t",
            choices = times_t(),
            selected = times_t()[1]
          )
        })
      }

      if (mode == "LIMITS AND EXPOSURE") {
        observe({
          toggle_id(input[["multiple_exposures"]], id = ns("points"))
          toggle_id(!input[["multiple_exposures"]], id = ns("t"))
        })

        t_out <- reactive({
          if (input[["multiple_exposures"]]) input[["points"]]
          else as.numeric(input[["t"]])
        })
      }

    }

    ### return values

    return(
      c(
        if (mode == "SINGLE POINT") list(
          t = input_r_numeric("t")
        ) else if (mode == "LIMITS AND SINGLE EXPOSURE") list(
          t = input_r_numeric("t"),
          `0` = time_0
        ) else list(
          points = input_r_numeric("points"),
          `0` = time_0,
          `100` = time_100
        ),
        if (mode == "LIMITS AND EXPOSURE") list(
          t = t_out,
          multiple = input_r("multiple_exposures")
        ) else NULL
      )
    )
  })
}

