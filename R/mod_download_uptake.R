#' download_uptake UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_uptake_ui <- function(id){
  ns <- NS(id)

  actionButton(
    inputId = ns("show_modal"),
    label = "Download for all peptides options... "
  )
}

#' download_uptake Server Functions
#'
#' @noRd
mod_download_uptake_server <- function(id, dat, params, peptide_table, s_time, s_calculation, s_visualization) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      showModal(
        modalDialog(
          title = "Download plot for all peptides",
          p(
            "Download uptake curves for all peptides in selected form,
            based on the parameters from the Settings panel.",
            "Preparing the plots may take a while."
          ),
          fluidRow(
            column_6(
              h5("Download a zipped folder with separate files:")
            ),
            column_6(
              h5("Download a PDF file with plots arranged in a grid:"),
              splitLayout(
                numericInput(
                  inputId = ns("num_cols"),
                  label = "Columns per page",
                  min = 1, max = 5,
                  value = 2
                ),
                numericInput(
                  inputId = ns("num_rows"),
                  label = "Rows per page",
                  min = 1, max = 5,
                  value = 2
                )

              )
            )
          ),
          fluidRow(
            column_6(
              downloadButton(
                outputId = ns("download_folder"),
                label = "Download"
              )
            ),
            column_6(
              downloadButton(
                outputId = ns("download_file"),
                label = "Download"
              )
            )
          ),
          footer = modalButton("Cancel"),
          size = "l"
        )
      )
    }) %>% bindEvent(input[["show_modal"]])

    plots_out <- reactive({
      all_peptides <- peptide_table() %>%
        select(Sequence, Start, End) %>%
        unique()

      # TODO: shouldn't we use States from settings here?
      states_download <- unique(peptide_table()[["State"]])

      withProgress(message = "Calculating data...", value = 0, max = 2 * nrow(all_peptides),
        all_peptides %>%
          mutate(title = glue::glue("{Sequence}({Start}-{End})")) %>%
          rowwise() %>%
          mutate(
            dat_uc = list({ val <- HaDeX::calculate_peptide_kinetics(
              dat(),
              protein  = params %()% chosen_protein,
              sequence = Sequence,
              start    = Start,
              end      = End,
              time_0   = s_time %()% 0,
              time_100 = s_time %()% 100
            ); incProgress(1); val}),
            plot_uc = list({ val <- HaDeX::plot_uptake_curve( # TODO: shouldn't there be differentiation between diff version (pun intended)?
              dat_uc,
              theoretical      = s_calculation   %()% theoretical,
              fractional       = s_calculation   %()% fractional,
              uncertainty_type = s_visualization %()% uncertainty_type,
              log_x            = s_visualization %()% log_x
            ) + ggtitle(title); incProgress(1); val})
          ) %>%
          pull(plot_uc)
      )
    })

    output[["download_file"]] <- downloadHandler(
      filename = "all_deut_uptake_curves.pdf",
      content = function(file) {
        plts_out <- plots_out()
        ggsave(
          filename = file,
          plot = withProgress(message = "Saving plots...", gridExtra::marrangeGrob(
            grobs = plts_out,
            ncol = input[["num_cols"]],
            nrow = input[["num_rows"]]
          )),
          device = "pdf",
          height = 300,
          width = 400,
          units = "mm"
        )
      }
    )

    output[["download_folder"]] <- downloadHandler(
      filename = "deut_uptake_curves.zip",
      content = function(file){
        previous_wd <- setwd(tempdir())
        on.exit(setwd(previous_wd))

        plts_out <- plots_out()

        plot_files <- withProgress(message = "Saving plots...", value = 0, max = length(plts_out),
          purrr::map_chr(plts_out, function(plt) {
            file_name <- glue::glue("{plt[['labels']][['title']]}.png")
            ggsave(
              filename = file_name,
              plot = plt,
              device = "png",
              bg = 'white',
              height = 300,
              width = 400,
              units = "mm"
            )
            incProgress(1)
            file_name
          })
        )

        zip(file, plot_files)
      })
  })
}
