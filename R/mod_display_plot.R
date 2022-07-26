#' plot_and_data_section UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_display_plot_ui <- function(id, plot_labels,
                                additional_data_info = NULL,
                                additional_plot_info = FALSE,
                                additional_button_server = NULL) {
  ns <- NS(id)
  div(
    class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
    fillCol(
      class = "HaDeX-plot-display-panel-container",
      flex = NA,
      if (length(plot_labels) == 1) tabsetPanel(
        tabPanel(
          title = plot_labels,
          girafeOutput_h(ns("plot")),
          verbatimTextOutput(ns("plot_info")) %nullify if% !additional_plot_info,
          downloadButton(ns("plot_download_button"), "Save chart (.svg)"),
          additional_button_server
        ),
        tabPanel(
          "Data",
          dataTableOutput_h(ns("data")),
          additional_data_info %?>% p
        )
      ) else purrr::imap(plot_labels, ~ tabsetPanel(
        tabPanel(
          title = .x,
          girafeOutput_h(ns(paste0("plot_", .y))),
          downloadButton(ns(paste0("plot_download_button_", .y)), "Save chart (.svg)")),
        tabPanel(
          title = "Data",
          dataTableOutput_h(ns(paste0("data_", .y))),
          additional_data_info %?>% (function(info) info[.y]) %?>% p
        )
      ))
    )
  )
}

#' plot_and_data_section Server Functions
#'
#' @importFrom ggiraph renderGirafe
#' @importFrom DT renderDataTable
#' @noRd
mod_display_plot_server <- function(id, plot_out, dat_out, info_out = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.reactive(plot_out)) {
      output[["plot"]] <- renderGirafe({ girafe(ggobj = plot_out(), width_svg = 17, height_svg = 9) })
      output[["data"]] <- renderDataTable({ HaDeX_DT_format(dat_out()) })
      output[["plot_download_button"]] <- downloadHandler(
        paste0(id, ".svg"),
        content = function(file) {
          ggsave(file, plot_out(), device = svg,
                 height = 300, width = 400, units = "mm")
        }
      )

      if (not_null(info_out)) {
        output[["plot_info"]] <- renderPrint({ info_out() })
      }
    } else {
      for (i in seq_along(plot_out)) {
        name <- names(plot_out)[i]
        rlang::inject({
          output[[paste0("plot_", !!name)]] <- renderGirafe({ girafe(ggobj = plot_out[[!!name]](), width_svg = 17, height_svg = 9) })
          output[[paste0("data_", !!name)]] <- renderDataTable({ HaDeX_DT_format(dat_out[[!!name]]()) })
          output[[paste0("plot_download_button_", !!name)]] <- downloadHandler(
            paste0(!!name, "-", id,".svg"),
            content = function(file) {
              ggsave(file, plot_out[[name]](), device = svg,
                     height = 300, width = 400, units = "mm")
            }
          )
        })
      }
    }

  })
}

