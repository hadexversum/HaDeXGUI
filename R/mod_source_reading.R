#' source_reading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import dplyr
#' @importFrom shiny NS tagList
mod_source_reading_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Input data",

    p(strong("Upload your file."), "Otherwise, you will see the example data."),
    p("Currently HaDeX is limited to `cluster` files from DynamX 3.0 or 2.0 and `tables` file from  HDeXaminer.",
      "The supplied file should contain at least two repetitions of the measurement for the uncertainty to be calculated.",
      "If the supplied file contains modified peptides, maximal exchange control cannot be applied."),
    p("Please be aware that loading data (including example file) may take a while. Thank you for the patience."),

    wellPanel(
      fillRow(
        flex = c(NA, 1),
        fileInput(
          inputId = ns("data_file"),
          label = "Choose file:",
          multiple = FALSE,
          accept = c(".csv", ".xlsx", ".xls"),
          placeholder = "No file selected"
        ),
        div(
          id = "HaDeX-file-status-panel",
          h6("File status:"),
          div(
            id = "HaDeX-file-status-message",
            verbatimTextOutput(ns("data_file_info"))
          )
        )
      )
    ),

    fillRow(
      id = "HaDeX-file-requirements-section",
      flex = c(1, NA),
      p("For the program to behave correctly, please make sure supplied file fulfills all requirements.",
        "Requirements can be displayed by clicking the button."),
      HaDeX_collapseButton(
        title = "Show requirements",
        target = "#HaDeX-file-requirements-table"
      )
    ),

    HaDeX_collapsablePanel(
      id = "HaDeX-file-requirements-table",
      htmlize_data("file_req")
    ),

    h3("Upload settings"),
    p("Values chosen here are propagated into all of the tabs for coherent results."),

    fillRow(
      id = "HaDeX-settings-panel",

      wellPanel(
        id = "HaDeX-standard-settings-panel",
        input_parameters_section(ns)
      ),

      undisplay(
        wellPanel(
          id = "HaDeX-examiner-settings-panel",
          mod_hdexaminer_adjustment_ui(ns("hdexaminer_adjustment"))
        )
      ),
      flex = c(NA, 1)
    )
  )
}

input_parameters_section <- function(ns) HaDeX_plotSettingsSection(

  title = "Select the parameters:",

  uiOutput(ns("gen_chosen_protein")),
  uiOutput(ns("gen_chosen_control")),
  uiOutput(ns("gen_no_deut_control")),
  numericInput_h(inputId = ns("deut_part"),
                 label = "Choose D20 concentration [%]: ",
                 value = 90,
                 min = 0, max = 100, step = 1,
                 width = "100%"),

  fluidRow(
    column(
      width = 6,
      numericInput_h(inputId = ns("sequence_start_shift"),
                     label = "Sequence start:",
                     value = 1, step = 1, min = 1,
                     width = "100%"),
    ),
    column(
      width = 6,
      uiOutput(ns("gen_sequence_length"))
    )
  ),
  verbatimTextOutput(ns("sequence_length_exp_info"))
)

#' source_reading Server Functions
#'
#' @importFrom icecream ic
#' @importFrom shinyvalidate InputValidator sv_gte sv_lte compose_rules
#' @noRd
mod_source_reading_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### reactive values

    dat_raw <- reactive({
      inFile <- input[["data_file"]]

      if (ic(is.null(inFile))){
        HaDeX::read_hdx("./inst/app/data/KD_180110_CD160_HVEM.csv")
      } else {
        validate(need(try(HaDeX::read_hdx(inFile[["datapath"]])), "Check file requirements!"))
        HaDeX::read_hdx(inFile[["datapath"]])
      }

    })

    data_source <- reactive({ attr(dat_raw(), "source") })

    dat_exam <- mod_hdexaminer_adjustment_server("hdexaminer_adjustment", dat_raw = dat_raw)

    dat_adjusted <- reactive({
      switch(data_source(),
        "HDeXaminer" = dat_exam(),
        dat_raw()
      ) %>%
        mutate(Start = Start + input[["sequence_start_shift"]] - 1,
               End = End + input[["sequence_start_shift"]] - 1)
    })

    dat <- reactive({
      req(
        input[["chosen_protein"]],
        input[["chosen_control"]]
      )

      # TODO: this function IS NOT exported from HaDeX due to empty line
      HaDeX:::create_control_dataset(dat = dat_adjusted(),
                                     control_protein = input[["chosen_protein"]],
                                     control_state = strsplit(input[["chosen_control"]], " \\| ")[[1]][2],
                                     control_exposure = strsplit(input[["chosen_control"]], " \\| ")[[1]][3])
    })


    has_modifications <- reactive({ attr(dat_adjusted(), "has_modification") })
    proteins_from_file <- reactive({ unique(dat_adjusted()[["Protein"]]) })
    states_from_file <- reactive({ unique(dat_adjusted()[["State"]]) })
    max_range_from_file <- reactive({
      req(input[["chosen_protein"]])
      max(filter(dat_adjusted(), Protein == input[["chosen_protein"]])[['End']])
    })
    times_from_file <- reactive({ sort(round(unique(dat_adjusted()[["Exposure"]]), digits = 3)) })
    # TODO: -\\-, i'm using times_from_file() bc of that
    times_with_control <- reactive({ setNames(times_from_file(), c(head(times_from_file(), -1), "chosen control")) })

    times_t <- reactive({ times_from_file()[times_from_file() > input[["no_deut_control"]] & times_from_file() < 99999] })
    max_range <- reactive({ max(max_range_from_file(), as.numeric(input[["sequence_length"]]), na.rm = TRUE) })

    options_for_control <- reactive({
      req(input[["chosen_protein"]])
      dat_adjusted() %>%
        filter(Protein == input[["chosen_protein"]]) %>%
        mutate(Exposure = round(Exposure, 4)) %>%
        select(Protein, State, Exposure) %>%
        arrange(State, desc(Exposure)) %>%
        unique(.) %>%
        mutate(control = paste0(Protein, " | ", State, " | ", Exposure)) %>%
        select(control)

    })

    states_chosen_protein <- reactive({
      req(input[["chosen_protein"]])
      dat_adjusted() %>%
        filter(Protein == input[["chosen_protein"]]) %>%
        select(State) %>%
        unique(.) %>%
        arrange(nchar(State)) %>%
        .[[1]]
    })

    ### ui outputs

    output[["gen_chosen_protein"]] <- renderUI({
      selectInput_h(inputId = ns("chosen_protein"),
                    label = "Choose protein: ",
                    choices = proteins_from_file(),
                    selected = proteins_from_file()[1],
                    width = "100%")
    })

    output[["gen_chosen_control"]] <- renderUI({
      selectInput_h(inputId = ns("chosen_control"),
                    label = "Maximal exchange control: ",
                    choices = options_for_control(),
                    width = "100%")
    })

    output[["gen_sequence_length"]] <- renderUI({
      numericInput_h(inputId = ns("sequence_length"),
                     label = "Sequence length:",
                     #TODO: is the hardcoded value relevant for ex. input?
                     value = max_range_from_file(), step = 1,
                     min = max_range_from_file(),
                     width = "100%")
    })

    output[["gen_no_deut_control"]] <- renderUI({
      req(times_from_file())
      no_deut_times <- times_from_file()[times_from_file() < 0.1]
      selectInput_h(inputId = ns("no_deut_control"),
                    label = "No deuterated time point:",
                    choices = no_deut_times,
                    selected = max(no_deut_times))
    })

    ### other outputs

    output[["data_file_info"]] <- renderText({
      status <- ""
      if (ic((is.null(input[["data_file"]])))){
        status <- "Example file: KD_180110_CD160_HVEM.csv."
      } else {
        length(dat_raw()[[1]])
        status <- "Supplied file is valid."
      }

      if(ic(data_source() == "HDeXaminer")){
        paste0(status, "\nDetected data source: ", data_source(), ". User action needed below!")
      } else {
        paste0(status, "\nDetected data source: ", data_source(), ".")
      }
    })

    output[["sequence_length_exp_info"]] <- renderText({ paste("Sequence length from the file is ", max_range_from_file(), ".") })

    ### observers

    observeEvent(data_source(), {
      golem::invoke_js(if (data_source() == "HDeXaminer") "show" else "hide", "#HaDeX-examiner-settings-panel")
    })

    ### validator

    iv <- InputValidator$new()
    iv$add_rule("sequence_start_shift", sv_gte(0))
    iv$add_rule("deut_part", sv_lte(100))
    iv$add_rule("sequence_length", compose_rules(
      ~ if (is.na(.)) "Must not contain `NA` values.",
      ~ if (. < max_range_from_file()) "Must be no shorther than any max end position in the file."
    ))
    iv$enable()
  })
}

## To be copied in the UI
# mod_source_reading_ui("source_reading_1")

## To be copied in the server
# mod_source_reading_server("source_reading_1")
