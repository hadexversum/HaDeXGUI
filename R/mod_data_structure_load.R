#' data_structure_load UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_structure_load_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Upload structure file!"),
    span(
      "If available, upload the pdb file with protein structure.",
      "Uptake data shown on the supplied structure is placed in the `Hires` part."
    ),
    fileInput(inputId = ns("structure_file"),
              label = "PDB file:",
              multiple = FALSE,
              accept = c(".cif", ".mmcif", ".pdb"),
              placeholder = "No file selected")

  )
}

#' data_structure_load Server Functions
#'
#' @noRd
mod_data_structure_load_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    structure_path <- reactive({

      input[["structure_file"]][["datapath"]]

    })

    return(structure_path)

  })
}

## To be copied in the UI
# mod_data_structure_load_ui("data_structure_load_1")

## To be copied in the server
# mod_data_structure_load_server("data_structure_load_1")
