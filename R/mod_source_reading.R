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
  )
}

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
