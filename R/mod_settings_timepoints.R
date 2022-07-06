#' settings_timepoints UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_timepoints_ui <- function(id, timepoints_switch = "show and deut"){
  ns <- NS(id)

  switch(
    timepoints_switch,
    `show and deut` = collapsible_card(
      title = "Timepoints",
      fluidRow(
        column(
          width = 6,
          checkboxGroupInput_h(
            inputId = ns("timepoints"),
            label = "Show time points: ",
            choices = "",
            selected = ""
          )
        ),
        column(
          width = 6,
          wrap_div(
            selectInput_h(
              inputId = ns("time_0"),
              label = "Deut 0%",
              choices = "",
              selected = ""
            ),
            id = ns("time_0"),
            type = "visswitch"
          ),
          wrap_div(
            selectInput_h(
              inputId = ns("time_100"),
              label = "Deut 100%",
              choices = "",
              selected = ""
            ),
            id = ns("time_100"),
            type = "visswitch"
          )
        )
      )
    ),
    `only deut` = wrap_div(
      collapsible_card(
        title = "Timepoints",
        splitLayout(
          selectInput_h(
            inputId = ns("time_0"),
            label = "Deut 0%",
            choices = "",
            selected = ""
          ),
          wrap_div(
            selectInput_h(
              inputId = ns("time_100"),
              label = "Deut 100%",
              choices = "",
              selected = ""
            ),
            id = ns("time_100"),
            type = "visswitch"
          )
        )
      ),
      id = ns("time_0"),
      type = "visswitch"
    )
  )
}

#' settings_timepoints Server Functions
#'
#' @noRd
mod_settings_timepoints_server <- function(id,
                                           p_times,
                                           p_times_with_control,
                                           p_no_deut_control,
                                           s_general) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### reactive values from inputs

    time_0 <- input_r_numeric("time_0")
    time_100 <- reactive({
      # this input is possibly missing due to not updating when it's hidden
      val <- as.numeric(input[["time_100"]])
      if (not_na(val)) val else NULL
    })

    ### observers updating inputs

    observe({
      updateSelectInput(
        session,
        inputId = "time_0",
        choices = p_times()[p_times() < MAX_TIME],
        selected = p_times()[p_times() == p_no_deut_control()]
      )

      toggle_id(
        !s_general[["theoretical"]](),
        wrap_id(ns("time_0"), "visswitch")
      )
    })

    observe({
      validate(need(length(p_times()) > 1, "Wait for parameters to be loaded"))

      updateSelectInput(
        session,
        inputId = "time_100",
        choices = p_times_with_control(),
        selected = max(p_times_with_control()[p_times_with_control() < MAX_TIME])
      )

      toggle_id(
        !s_general[["theoretical"]]() && s_general[["fractional"]](),
        wrap_id(ns("time_100"), "visswitch")
      )
    })

    observe({
      vec <- if (s_general[["fractional"]]()){
        validate(need(not_null(time_100()),
                      "Wait for parameters to be loaded"))

        p_times() < time_100()
      } else
        p_times() < MAX_TIME

      times_t <- p_times()[p_times() > time_0() & vec]

      updateCheckboxGroupInput(
        session,
        inputId = "timepoints",
        choices = times_t,
        selected = times_t
      )
    })

    ### return values

    return(
      list(
        timepoints = input_r_numeric("timepoints"),
        time_0 = time_0,
        time_100 = time_100
      )
    )
  })
}

