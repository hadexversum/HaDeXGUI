#' hdexaminer_adjustment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_hdexaminer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("File from HDeXaminer detected!"),
    span(
      "Some of the information from the data file requires your confirmation.",
      "For the additional information on how the data from HDeXaminer is processed, check the requirements above.",
      "Keep in mind that the MHP value is generated based on the peptide sequence and therefore, may differ from actual value in case of the modifications."
    ),
    numericInput_h(
      inputId = ns("fd_timepoint"),
      label = "FD timepoint [min]:",
      value = 1440,
      min = 0, #TODO: setup a validator?
      width = "100%"
    ),
    textInput_h(
      inputId = ns("protein_name"),
      label = "Protein name:",
      width = "100%"
    ),
    textInput_h(
      inputId = ns("state_names"),
      label = "State names:",
      width = "100%"
    ),
    checkboxGroupInput_h(
      inputId = ns("confidence"),
      label = "Accepted confidence values:",
      choices = c("High", "Medium", "Low"),
      selected = c("Medium", "High")
    ),
    actionButton(inputId = ns("apply"),
                 label = "Apply changes to continue"),
    span(
      "The calculated values of MPH might slightly differ based on data used and its precision."
    ),
    a(
      href = "http://www.matrixscience.com/help/aa_help.html",
      "Used amino mass data"
    ),
    DT::dataTableOutput(ns("updated_data"))
  )
}

#' hdexaminer_adjustment Server Functions
#'
#' @param dat_raw raw data file passed from source_reading module
#'
#' @noRd
mod_data_hdexaminer_server <- function(id, dat_raw) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### reactive values

    proteins_from_file <- reactive({ unique(dat_raw()[["Protein"]]) })
    states_from_file <- reactive({ unique(dat_raw()[["State"]]) })

    # this invalidation flag is to be sure that every time a new file is
    # uploaded, confirmation of changes is required
    invalidation_flag <- reactiveVal(0)
    bindEvent(observe({ invalidation_flag(0) }),
              dat_raw())
    bindEvent(observe({ invalidation_flag(invalidation_flag() + 1) }),
              input[["apply"]])

    dat_exam <- bindEvent(reactive({
      validate(need(invalidation_flag() > 0, "Apply changes in `Input Data` tab."))

      # TODO: do something with the messages
      get_internal_messages(HaDeX::update_hdexaminer_file(
        dat = dat_raw(),
        fd_time = input[["fd_timepoint"]],
        old_protein_name = proteins_from_file(),
        new_protein_name = input[["protein_name"]],
        old_state_name = states_from_file(),
        new_state_name = strsplit(input[["state_names"]], ",")[[1]],
        confidence = input[["confidence"]]))
    }), invalidation_flag())

    ### observers updating ui

    observe({
      updateTextInput(
        session = session,
        inputId = "protein_name",
        value = proteins_from_file()
      )
    })

    observe({
      updateTextInput(
        session = session,
        inputId = "state_names",
        value = paste(states_from_file(), collapse = ", "),
      )
    })

    ### other outputs

    output[["updated_data"]] <- DT::renderDataTable({

      dat <- unique(dat_exam()[, .(Protein, State, Sequence,  Start, End, MHP)])
      setorderv(dat, cols = c("Start", "End"))
      dat

    })

    ### return values

    return(
      dat_exam
    )
  })
}
