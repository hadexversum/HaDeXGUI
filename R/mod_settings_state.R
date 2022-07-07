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
mod_settings_state_server <- function(id, differential, p_states_chosen_protein){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (differential) {
      observe({
        updateSelectInput(
          session,
          inputId = "state_1",
          choices = p_states_chosen_protein(),
          selected = p_states_chosen_protein()[1]
        )
      })

      observe({
        not_state_1 <- setdiff(p_states_chosen_protein(), input[["state_1"]])

        updateSelectInput(
          session,
          inputId = "state_2",
          choices = not_state_1,
          selected = not_state_1[1]
        )
      })
    } else {
      observe({
        updateSelectInput(
          session,
          inputId = "state",
          choices = p_states_chosen_protein(),
          selected = p_states_chosen_protein()[1]
        )
      })
    }

    return(
      if (differential)
        list(
          state_1 = reactive({
            wait_for(input[["state_1"]] %in% p_states_chosen_protein())

            input[["state_1"]]
          }),
          state_2 = reactive({
            wait_for(input[["state_1"]] != input[["state_2"]])
            input[["state_2"]]
          })
        )
      else
        list(
          state = reactive({
            validate(need(input[["state"]] %in% p_states_chosen_protein(),
                          "Wait for the parameters to be loaded."))

            input[["state"]]
          })
        )
    )
  })
}
