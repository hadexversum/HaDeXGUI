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
        mod_settings_applying_ui(ns("settings_applying"))
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
      data_file <- input[["data_file"]]

      if (is.null(data_file)) {
        HaDeX::read_hdx("./inst/app/data/KD_180110_CD160_HVEM.csv")
      } else {
        validate(need(try({
          file <- HaDeX::read_hdx(data_file[["datapath"]])
        }), "File does not fullfill requirements. Check file requirements!"))
        file
      }
    })

    data_source <- reactive({ attr(dat_raw(), "source") })

    dat_exam <- mod_hdexaminer_adjustment_server("hdexaminer_adjustment", dat_raw = dat_raw)

    dat_adjusted <- reactive({
      switch(data_source(),
        "HDeXaminer" = dat_exam(),
        dat_raw()
      )
    })

    ### other outputs

    output[["data_file_info"]] <- renderText({
      paste0(
        if (is.null(input[["data_file"]]))
          "Example file: KD_180110_CD160_HVEM.csv."
        else "Supplied file is valid.",
        "\nDetected data source: ",
        if (data_source() == "HDeXaminer")
          ". User action needed below!"
        else "."
      )
    })

    ### observer

    observe({
      golem::invoke_js(
        if (data_source() == "HDeXaminer") "show" else "hide",
        "#HaDeX-examiner-settings-panel"
      )
    })

    ### return values

    return(
      mod_settings_applying_server("settings_applying", dat_adjusted = dat_adjusted)
    )
  })
}
