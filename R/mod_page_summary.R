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

    dat_processed <- reactive({
      HaDeX::show_summary_data(
        dat = dat(),
        confidence_limit_1 = input[["confidence_limit"]], #TODO: why does it exist?
        confidence_limit_2 = input[["confidence_limit_2"]],
        protein_length = params %()% max_range
      )
    })

    output[["table"]] <- DT::renderDataTable(server = FALSE, {
      HaDeX_DT_format(dat_processed())
    })

    return(
      dat_processed
    )
  })
}
