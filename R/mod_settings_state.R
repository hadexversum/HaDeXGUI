#' settings_state UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_state_ui <- function(id, differential){
  ns <- NS(id)

  if (differential)
    collapsible_card(
      title = "States",
      p("Differential plot presents the uptake difference between State 1 and State 2."),
      splitLayout(
        selectInput_h(
          inputId = ns("state_1"),
          label = "State 1",
          choices = ""
        ),
        selectInput_h(
          inputId = ns("state_2"),
          label = "State 2",
          choices = ""
        )
      )
    )
  else
    collapsible_card(
      title = "State",
      selectInput_h(
        inputId = ns("state"),
        label = "Choose state:",
        choices = "",
        selected = ""
      )
    )
}

#' settings_state Server Functions
#'
#' @noRd
mod_settings_state_server <- function(id, differential, states_chosen_protein){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (differential) {
      observe({
        updateSelectInput(
          session,
          inputId = "state_1",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[1]
        )
      })

      observe({
        updateSelectInput(
          session,
          inputId = "state_2",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[2]
        )
      })
    } else {
      observe({
        updateSelectInput(
          session,
          inputId = "state",
          choices = states_chosen_protein(),
          selected = states_chosen_protein()[1]
        )
      })
    }

    return(
      if (differential)
        input_rv(c("state_1", "state_2"))
      else
        input_rv("state")
    )
  })
}
