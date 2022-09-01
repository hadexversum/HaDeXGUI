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
  stopifnot(mode %in% c("SINGLE", "MULTIPLE"))
  ns <- NS(id)
  collapsible_card(
    title = "Peptide",

    p(
      c(
        SINGLE = "Select peptide:",
        MULTIPLE = "Select one or more peptides:"
      )[mode]
    ),
    dataTableOutput_h(ns("peptide_list")),
    secondary_button(
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
  stopifnot(mode %in% c("SINGLE", "MULTIPLE"))
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[["peptide_list"]] <- DT::renderDataTable({
      DT::datatable(
        data = peptide_table(),
        class = "table-bordered table-condensed",
        extensions = "Buttons",
        selection = c(SINGLE = "single", MULTIPLE = "multiple")[mode],
        options = list(pageLength = 10, dom = "tip", autoWidth = TRUE, target = 'cell'),
        filter = "bottom",
        rownames = FALSE
      )
    })

    peptide_list_proxy <- DT::dataTableProxy("peptide_list", session = session)

    observe({
      bindEvent(DT::selectRows(peptide_list_proxy, NULL),
                input[["reset"]])
    })

    return(
      list(
        selected = reactive({
          validate(need(input[["peptide_list_rows_selected"]],
                        c(
                          SINGLE = "Please select one peptide from the table on the left.",
                          MULTIPLE = "Please select at least one peptide from the table on the left."
                        )[mode]
          ))
          input[["peptide_list_rows_selected"]]
        }),
        selected_flag = reactive({ length(input[["peptide_list_rows_selected"]]) > 0})
      )
    )
  })
}
