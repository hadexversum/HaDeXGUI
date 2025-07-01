#' export_hdxviewer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_hdxviewer_ui <- function(id){
  ns <- NS(id)

  actionButton(
    inputId = ns("show_hdxviewer_panel"),
    label = "Export data for HDXViewer"
  )
}

#' export_hdxviewer Server Functions
#'
#' @noRd
mod_export_hdxviewer_server <- function(id, hdxviewer_dat, params, content, differential = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      showModal(
        modalDialog(
          title = "Export data for HDX-Viewer",
          p("The data is not transfered automatically, please do it manually."),
          wellPanel(
            selectInput(inputId = ns("diff_viewer_datatype"),
                        label = "Select data to import:",
                        choices = c("Aggregated deuterium uptake"),
                        selected = c("Aggregated deuterium uptake")),
            downloadButton(outputId = ns("diff_viewer_download_button"),
                           label = "Create file"),
            actionButton(inputId = ns("diff_open_viewer"),
                         label = "Open HDXviewer",
                         icon = icon("th"),
                         onclick ="window.open('http://proteotoul.ipbs.fr:8080/')")
          ),
          p("How to use HDXViewer?"),
          p("Open the HDXViewer by clicking the button above. Then, select `HDX data type`:  HDExaminer/csv. Click on the button `Upload PDB File` and select appropriate file. Select chain. Then, click the button `Upload CSV Files` and select the newly created file (obtained by clicking the `Create file` button above). Adjust the Viewer options and enjoy the results! ")
        )
      )
    }) %>% bindEvent(input[["show_hdxviewer_panel"]])



  # download file

  output[["diff_viewer_download_button"]] <- downloadHandler(

    filename = paste0(params %()% chosen_protein, "-", content, ".csv"),
    content = function(file){
                write.csv(hdxviewer_dat(),
                file = file,
                quote = FALSE,
                row.names = FALSE)}
    )



  })
}

## To be copied in the UI
# mod_export_hdxviewer_ui("mod_export_hdxviewer_1")
# mod_export_hdxviewer_ui(ns("export_hdxviewer"))

## To be copied in the server
# mod_export_hdxviewer_server("mod_export_hdxviewer_1")
# mod_export_hdxviewer_server(ns("export_hdxviewer"))
# mod_export_hdxviewer_server("export_hdxviewer")
