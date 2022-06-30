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
    numericInput_h(inputId = ns("examiner_fd_timepoint"),
                   label = "FD timepoint [min]:",
                   value = 1440,
                   min = 0,
                   width = "100%"),
    uiOutput(ns("gen_exam_protein_name")),
    uiOutput(ns("gen_exam_state_name")),
    checkboxGroupInput_h(inputId = ns("exam_confidence"),
                         label = "Accepted confidence values:",
                         choices = c("High", "Medium", "Low"),
                         selected = c("Medium", "High")),
    actionButton(inputId = ns("exam_apply_changes"),
                 label = "Apply changes to continue"),
    span(
      "The calculated values of MPH might slightly differ based on data used and its precision."
    ),
    a(
      href = "http://www.matrixscience.com/help/aa_help.html",
      "Used amino mass data"
    ),
    DT::dataTableOutput(ns("checking_exam_data"))
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

    proteins_from_file <- reactive({ ic(unique(dat_raw()[["Protein"]])) })
    states_from_file <- reactive({ unique(dat_raw()[["State"]]) })

    # this invalidation flag is to be sure that every time a new file is
    # uploaded, confirmation of changes is required
    invalidation_flag <- reactiveVal(0)
    observe({ invalidation_flag(0) }) %>%
      bindEvent(dat_raw())
    observe({ invalidation_flag(invalidation_flag() + 1) }) %>%
      bindEvent(input[["exam_apply_changes"]])

    dat_exam <- reactive({
      validate(need(invalidation_flag() > 0, "Apply changes in `Input Data` tab."))

      # TODO: do something with the messages
      get_internal_messages(HaDeX::update_hdexaminer_file(
        dat = dat_raw(),
        fd_time = input[["examiner_fd_timepoint"]],
        old_protein_name = proteins_from_file(),
        new_protein_name = input[["exam_protein_name"]],
        old_state_name = states_from_file(),
        new_state_name = strsplit(input[["exam_state_name"]], ",")[[1]],
        confidence = input[["exam_confidence"]]))
    }) %>% bindEvent(invalidation_flag())


    ### ui outputs

    output[["gen_exam_protein_name"]] <- renderUI({
      textInput_h(inputId = ns("exam_protein_name"),
                  label = "Protein name:",
                  value = proteins_from_file(),
                  width = "100%")
    })

    output[["gen_exam_state_name"]] <- renderUI({
      textInput_h(inputId = ns("exam_state_name"),
                  label = "States names:",
                  value = paste(states_from_file(), collapse = ", "),
                  width = "100%")
    })

    ### other outputs

    output[["checking_exam_data"]] <- DT::renderDataTable({
      dat_exam() %>%
        select(Protein, State, Sequence,  Start, End, MHP) %>%
        unique(.) %>%
        arrange(Start, End)
    })

    ### return values

    return(
      dat_exam
    )
  })
}