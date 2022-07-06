#' settings_peptide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_peptide_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Peptide",

    p("Choose peptide:"),
    dataTableOutput_h("peptide_list"),
    actionButton(
      inputId = "reset",
      label = "Reset chosen peptides"
    )
  )
}

#' settings_peptide Server Functions
#'
#' @noRd
mod_settings_peptide_server <- function(id, dat, p_chosen_protein){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    peptides <- reactive({
      dat() %>%
        filter(Protein == p_chosen_protein()) %>%
        select(Sequence, State, Start, End) %>%
        unique(.) %>%
        arrange(Start, End)

    })

    output[["peptide_list"]] <- DT::renderDataTable({
      datatable(
        data = peptides(),
        class = "table-bordered table-condensed",
        extensions = "Buttons",
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
      input_r_list("peptide_list_rows_selected")
    )
  })
}

## To be copied in the UI
# mod_settings_peptide_ui("settings_peptide_1")

## To be copied in the server
# mod_settings_peptide_server("settings_peptide_1")