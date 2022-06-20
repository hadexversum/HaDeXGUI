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

  })
}

## To be copied in the UI
# mod_source_reading_ui("source_reading_1")

## To be copied in the server
# mod_source_reading_server("source_reading_1")
