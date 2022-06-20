#' source_reading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
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
      tableOutput(ns("file_req"))
    ),

    h3("Upload settings"),
    p("Values chosen here are propagated into all of the tabs for coherent results."),

    fillRow(
      id = "HaDeX-settings-panel",
      wellPanel(
        id = "HaDeX-standard-settings-panel",
        input_parameters()
      ),

      shinyjs::hidden(
        wellPanel(
          id = "HaDeX-examiner-settings-panel",
          h3("File from HDeXaminer detected!"),
          span(
            "Some of the information from the data file requires your confirmation.",
            "For the additional information on how the data from HDeXaminer is processed, check the requirements above.",
            "Keep in mind that the MHP value is generated based on the peptide sequence and therefore, may differ from actual value in case of the modifications."
          ),
          numericInput_h(inputId = "examiner_fd_timepoint",
                         label = "FD timepoint [min]:",
                         value = 1440,
                         min = 0,
                         width = "100%"),
          textInput_h(inputId = "exam_protein_name",
                      label = "Protein name:",
                      width = "100%"),
          textInput_h(inputId = "exam_state_name",
                      label = "States names:",
                      width = "100%"),
          checkboxGroupInput_h(inputId = "exam_confidence",
                               label = "Accepted confidence values:",
                               choices = c("High", "Medium", "Low"),
                               selected = c("Medium", "High")),
          actionButton(inputId = "exam_apply_changes",
                       label = "Apply changes to continue"),
          span(
            "The calculated values of MPH might slightly differ based on data used and its precision."
          ),
          a(
            href = "http://www.matrixscience.com/help/aa_help.html",
            "Used amino mass data"
          ),
          DT::dataTableOutput("checking_exam_data"),
        )
      ),
      flex = c(NA, 1)
    )
  )
}

input_parameters <- function() HaDeX_plotSettingsSection(

  title = "Select the parameters:",

  selectInput_h(inputId = "chosen_protein",
                label = "Choose protein: ",
                choices = c("db_CD160"),
                width = "100%"),
  selectInput_h(inputId = "chosen_control",
                label = "Maximal exchange control: ",
                choices = c("not present", "db_CD160 | CD160 | 1440"),
                width = "100%"),
  selectInput_h(inputId = "no_deut_control",
                label = "No deuterated time point:",
                choices = c(0, 0.001, 5),
                selected = 0.001),
  numericInput_h(inputId = "deut_part",
                 label = "Choose D20 concentration [%]: ",
                 value = 90,
                 min = 0, max = 100, step = 1,
                 width = "100%"),

  fluidPage(
    fluidRow(
      column(
        width = 6,
        numericInput_h(inputId = "sequence_start_shift",
                       label = "Sequence start:",
                       value = 1, step = 1,
                       width = "100%"),
      ),
      column(
        width = 6,
        numericInput_h(inputId = "sequence_length",
                       label = "Sequence length:",
                       value = 300, step = 1,
                       width = "100%")
      )
    )
  ),
  textOutput("sequence_length_exp_info")


)

#' source_reading Server Functions
#'
#' @importFrom icecream ic
#' @noRd
mod_source_reading_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dat_in <- reactive({
      inFile <- input[["data_file"]]

      if (ic(is.null(inFile))){
        HaDeX::read_hdx("./inst/app/data/KD_180110_CD160_HVEM.csv")
      } else {
        validate(need(try(HaDeX::read_hdx(inFile[["datapath"]])), "Check file requirements!"))
        HaDeX::read_hdx(inFile[["datapath"]])
      }

    })

    data_source <- reactive({ attr(dat_in(), "source") })

    output[["data_file_info"]] <- renderText({
      status <- ""
      if (ic((is.null(input[["data_file"]])))){
        status <- "Example file: KD_180110_CD160_HVEM.csv."
      } else {
        length(dat_in()[[1]])
        status <- "Supplied file is valid."
      }

      if(ic(data_source() == "HDeXaminer")){
        paste0(status, "\nDetected data source: ", data_source(), ". User action needed below!")
      } else {
        paste0(status, "\nDetected data source: ", data_source(), ".")
      }
    })

    output[["file_req"]] <- renderTable({
      data.frame(
        Name = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
        Type = c("Character", "Integer", "Integer", "Character", "Character", "Character", "Numeric", "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric"),
        Obligatory = c("TRUE", "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE"),
        Description = c("Protein name", "Peptide's first amino acid position in sequence", "Peptide's last amino acid position in sequence", "Peptide's sequence in single amino acid in one letter code", "Post-transitional modification of the peptyde", "Fragment label from ETD-HDX data", "Number od maximal deuteriul atoms uptake", "mass of the singly charged molecular ion", "Name od the protein state", "D2O exposure in minutes", "Name of .raw file", "Charge", "Peptide's retention time in minures", "Intensity", "Measured mass to charge ratio"),
        DynamX3.0 = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
        DynamX2.0 = c("Protein", "Start", "End", "Sequence", "No information", "No information", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
        HDeXaminer = c("Shortest value from `Protein State`", "Start", "End", "Sequence", "No information", "No information", "Calculated based on sequence", "Calculated based on sequence", "Protein State", "Deut Time", "Experiment", "Charge", "Search RT", "Max Inty", "Exp Cent")
      )
    })

  })
}

## To be copied in the UI
# mod_source_reading_ui("source_reading_1")

## To be copied in the server
# mod_source_reading_server("source_reading_1")
