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
  stopifnot(mode %in% c("DISABLED", "SINGLE", "DOUBLE", "MULTIPLE"))
  ns <- NS(id)

  collapsible_card(
    title = if (mode == "SINGLE") "State" else "States",
    switch(
      mode,
      DOUBLE = tagList(
        p("Differential plot presents the uptake difference between State 1 and State 2."),
        splitLayout(
          selectizeInput_h(
            inputId = ns("state_1"),
            label = "State 1"
          ),
          selectizeInput_h(
            inputId = ns("state_2"),
            label = "State 2"
          )
        )
      ),
      SINGLE = selectizeInput_h(
        inputId = ns("state"),
        label = "Choose state:"
      ),
      MULTIPLE = checkboxGroupInput_h(
        inputId = ns("states"),
        label = "Choose states for comparison:"
      )
    ),
    fancy_icon = "atom"
  ) %.?!% (mode == "DISABLED")
}

#' settings_state Server Functions
#'
#' @noRd
mod_settings_state_server <- function(id, mode,
                                      p_states_chosen_protein){
  stopifnot(mode %in% c("DISABLED", "SINGLE", "DOUBLE", "MULTIPLE"))
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    if (mode == "DOUBLE") {
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
    } else if (mode == "SINGLE") {
      observe({
        updateSelectInput(
          session,
          inputId = "state",
          choices = p_states_chosen_protein(),
          selected = p_states_chosen_protein()[1]
        )
      })
    } else if (mode == "MULTIPLE") {
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
      if (mode == "MULTIPLE") list(
        states = reactive({
          wait_for(all(input[["states"]] %in% p_states_chosen_protein()))
          validate(need(length(input[["states"]]) > 0,
                        "Please select at least one state."))
          input[["states"]]
        })
      ) else if (mode == "DOUBLE") list(
        state_1 = reactive({
          wait_for(input[["state_1"]] %in% p_states_chosen_protein())
          input[["state_1"]]
        }),
        state_2 = reactive({
          wait_for(input[["state_1"]] != input[["state_2"]])
          input[["state_2"]]
        })
      ) else if (mode == "SINGLE") list(
        state = reactive({
          wait_for(input[["state"]] %in% p_states_chosen_protein())
          input[["state"]]
        })
      )
    )
  }) %.?!% (mode == "DISABLED")
}
