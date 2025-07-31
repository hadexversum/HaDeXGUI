#' @noRd
viewer3dmolUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("structure_info"),
        p("Load .mmcif/.pdb file in the Input data section to see results on the structure!")
    ),
    div(
      id = ns("structure-options"),
      tags$button("Create structure creenshot", id = ns("btn"), `data-screenshot` = ns("viewer"), class = "btn-default"),
      tags$input(type = "checkbox", id = ns("spin"), checked = "checked", `data-spin` = ns("viewer")),
      tags$label(" Spin structure", `for` = ns("spin")),
      style = "margin-bottom: 10px;",
      hidden = TRUE,
    ),
    div(id = ns("viewer"),
        `structure-viewer` = NA,
        style = "width:100%; height:700px; margin-bottom: 30px;")
  )
}

#' @noRd
viewer3dmolServer <- function(id, file_path, color_map, chosen_protein) {
  moduleServer(id, function(input, output, session) {

    observe({
    if(is.null(file_path())) {
      shinyjs::hide(id = "structure-options")
      shinyjs::show(id = "structure_info")
    }
    })

    observe({
    if(!is.null(file_path())) {
      shinyjs::show(id = "structure-options")
      shinyjs::hide(id = "structure_info")
    }
    })

    ##

    observeEvent(file_path(), {
      file <- file_path()
      req(file)

      observeEvent(color_map(), {
        session$sendCustomMessage("renderStructure", list(
          containerId = session$ns("viewer"),
          data = paste(readLines(file), collapse = "\n"),
          colorMap = as.list(color_map()),
          protName = chosen_protein()
      ))

      })

    })
  })
}
