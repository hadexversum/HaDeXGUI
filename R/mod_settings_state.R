#' settings_state UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_state_ui <- function(id, mode){
  stopifnot(mode %in% c("disabled", "single", "double", "multiple"))
  ns <- NS(id)

  collapsible_card(
    title = if (mode == "single") "State" else "States",
    switch(
      mode,
      double = tagList(
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
      ),
      single = selectInput_h(
        inputId = ns("state"),
        label = "Choose state:",
        choices = "",
        selected = ""
      ),
      multiple = checkboxGroupInput_h(
        inputId = ns("states"),
        label = "Choose states for comparison:",
        choices = "",
        selected = ""
      )
    ),
    fancy_icon = "atom"
  ) %nullify if% (mode == "disabled")
}

#' settings_state Server Functions
#'
#' @noRd
mod_settings_state_server <- function(id, mode,
                                      p_states_chosen_protein){
  stopifnot(mode %in% c("disabled", "single", "double", "multiple"))
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    if (mode == "double") {
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
    } else if (mode == "single") {
      observe({
        updateSelectInput(
          session,
          inputId = "state",
          choices = p_states_chosen_protein(),
          selected = p_states_chosen_protein()[1]
        )
      })
    } else if (mode == "multiple") {
      observe({
        updateCheckboxGroupInput(
          session,
          inputId = "states",
          choices = p_states_chosen_protein(),
          selected = p_states_chosen_protein()
        )
      })
    }

    return(
      if (mode == "multiple") list(
        states = reactive({
          wait_for(all(input[["states"]]) %in% p_states_chosen_protein())
          input[["states"]]
        })
      ) else if (mode == "double") list(
        state_1 = reactive({
          wait_for(input[["state_1"]] %in% p_states_chosen_protein())
          input[["state_1"]]
        }),
        state_2 = reactive({
          wait_for(input[["state_1"]] != input[["state_2"]])
          input[["state_2"]]
        })
      ) else if (mode == "single") list(
        state = reactive({
          wait_for(input[["state"]] %in% p_states_chosen_protein())
          input[["state"]]
        })
      )
    )
  }) %nullify if% (mode == "disabled")
}
