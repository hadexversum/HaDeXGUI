#' settings_peptide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_peptide_ui <- function(id, peptide_mode){
  ns <- NS(id)
  collapsible_card(
    title = "Peptide",

    p(
      if (peptide_mode == "peptide and state") "Select peptides and according states:"
      else if (peptide_mode == "single peptide") "Select peptide"
    ),
    dataTableOutput_h(ns("peptide_list")),
    actionButton(
      inputId = ns("reset"),
      label = "Reset selection"
    )
  )
}

#' settings_peptide Server Functions
#'
#' @noRd
mod_settings_peptide_server <- function(id, dat, peptide_table, peptide_mode){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[["peptide_list"]] <- DT::renderDataTable({
      DT::datatable(
        data = peptide_table(),
        class = "table-bordered table-condensed",
        extensions = "Buttons",
        selection = if (peptide_mode == "peptide and state") "multiple" else if (peptide_mode == "single peptide") "single",
        options = list(pageLength = 10, dom = "tip", autoWidth = TRUE, target = 'cell'),
        filter = "bottom",
        rownames = FALSE
      )
    })

    peptide_list_proxy <- DT::dataTableProxy("peptide_list", session = session)

    observe({
      DT::selectRows(peptide_list_proxy, NULL)
    }) %>% bindEvent(input[["reset"]])

    return(
      list(
        selected = input_r("peptide_list_rows_selected")
      )
    )
  })
}
