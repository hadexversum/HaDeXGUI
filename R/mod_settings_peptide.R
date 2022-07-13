#' settings_peptide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_peptide_ui <- function(id, mode){
  stopifnot(mode %in% c("single peptide", "multiple peptides and states"))
  ns <- NS(id)
  collapsible_card(
    title = "Peptide",

    p(
      if (mode == "multiple peptides and states") "Select peptides and according states:"
      else if (mode == "single peptide") "Select peptide"
    ),
    dataTableOutput_h(ns("peptide_list")),
    actionButton(
      inputId = ns("reset"),
      label = "Reset selection"
    ),
    fancy_icon = "dna"
  )
}

#' settings_peptide Server Functions
#'
#' @noRd
mod_settings_peptide_server <- function(id, peptide_table, mode){
  stopifnot(mode %in% c("single peptide", "multiple peptides and states"))
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[["peptide_list"]] <- DT::renderDataTable({
      DT::datatable(
        data = peptide_table(),
        class = "table-bordered table-condensed",
        extensions = "Buttons",
        selection = if (mode == "multiple peptides and states") "multiple" else if (mode == "single peptide") "single",
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
