#' plot_and_data_section UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_display_plot_section_ui <- function(id, plot_label, additional_data_info = NULL) {
  ns <- NS(id)
  div(
    class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
    fillCol(
      class = "HaDeX-plot-display-panel-container",
      flex = NA,
      tabsetPanel(
        tabPanel(
          plot_label,
          girafeOutput_h(ns("plot")),
          downloadButton(ns("plot_download_button"), "Save chart (.svg)")),
        tabPanel(
          "Data",
          DT::dataTableOutput(ns("data")),
          additional_data_info %?>% p
        )
      )
    )
  )
}

#' plot_and_data_section Server Functions
#'
#' @importFrom ggiraph renderGirafe
#' @importFrom DT renderDataTable
#' @noRd
mod_display_plot_section_server <- function(id, plot_out, dat_out) {
  moduleServer(id, function(input, output, session) {
    output[["plot"]] <- renderGirafe({ girafe(ggobj = plot_out(), width_svg = 17, height_svg = 9) })

    output[["data"]] <- renderDataTable({ HaDeX_DT_format(dat_out()) })

    output[["plot_download_button"]] <- downloadHandler(
      "butterfly_plot.svg",
      content = function(file) {
        ggsave(file, plot_out(), device = svg,
               height = 300, width = 400, units = "mm")
      }
    )
  })
}

## To be copied in the UI
# mod_display_plot_section_ui("plot_and_data_section_1")

## To be copied in the server
# mod_display_plot_section_server("plot_and_data_section_1")
