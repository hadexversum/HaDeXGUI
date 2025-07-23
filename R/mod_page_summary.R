#' page_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_summary_ui <- function(id){
  ns <- NS(id)
  hadex_tab_other(
    title = "Summary",

    checkboxGroupInput_h(
      inputId = ns("state"),
      label = "Choose state:"
    ),

    dataTableOutput_h(ns("table"), width = "60%"),
    includeMarkdown(app_sys("app/man/summary.md"))
  )
}

#' page_summary Server Functions
#'
#' @noRd
mod_page_summary_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateCheckboxGroupInput(
        session,
        inputId = "state",
        choices = unique(dat()[["State"]]),
        selected = unique(dat()[["State"]])
      )
    })

    dat_processed <- reactive({

      validate(need(length(input[["state"]])>0, "Choose at least one state"))

      dat() %>% filter(State %in% input[["state"]]) %>%
      HaDeX::show_summary_data(
        dat = .,
        confidence_level = 0.9, #TODO: fix this hardcoded value!
        protein_length = params %()% max_range
      )
    })

    output[["table"]] <- DT::renderDataTable(server = FALSE, {
      hadex_datatable(dat_processed(), dom = "tB", filename = "summary")
    })

    return(
      dat_processed
    )
  })
}
