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
mod_export_hdxviewer_server <- function(id, dat, params, s_test, s_time, s_calculation, s_woods_state, dat_processed_woods){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      showModal(
        modalDialog(
          title = "Export data for HDX-Viewer",
          p("If the data is not transfer automatically, please do it manually."),
          wellPanel(
            h3("Export data for HDX Viewer"),
            h6("Feature under construction"),
            selectInput(inputId = ns("diff_viewer_chain"),
                        label = "Select chain:",
                        choices = c("A", "B"),
                        selected = "A"),
            selectInput(inputId = ns("diff_viewer_datatype"),
                        label = "Select data to import",
                        choices = c("Hybrid significance"),
                        selected = c("Hybrid significance")),
            h6("The first amino acid of the peptide is omitted."),
            downloadButton(outputId = ns("diff_viewer_download_button"),
                           label = "Create file"),
            # actionButton(inputId = ns("diff_open_viewer"),
            #              label = "Open HDXviewer",
            #              icon = icon("th")),
            h6("Automated loading may not be working properly. In that case, download and upload the files manually. ")
          )
        )
      )
    }) %>% bindEvent(input[["show_hdxviewer_panel"]])

    ## service

    woods_test_results <- reactive({

       HaDeX::create_p_diff_uptake_dataset(dat                 = dat(),
                                           diff_uptake_dat     = dat_processed_woods(), ##
                                           protein             = params        %()% chosen_protein,
                                           state_1             = s_woods_state %()% state_1,
                                           state_2             = s_woods_state %()% state_2,
                                           p_adjustment_method = s_test        %()% p_adjustment_method,
                                           confidence_level    = s_test        %()% confidence_level,
                                           time_0              = s_time        %()% 0,
                                           time_100            = s_time        %()% 100,
                                           deut_part           = params        %()% deut_part) %>%
        HaDeX::create_p_diff_uptake_dataset_with_confidence(p_diff_uptake_dat = .,
                                                            theoretical = s_calculation   %()% theoretical,
                                                            fractional  = s_calculation   %()% fractional) %>%
        HaDeX::calculate_aggregated_test_results(p_diff_uptake_conf_dat = .,
                                                 time_t = s_time %()% t)

    })

  # download file

  output[["diff_viewer_download_button"]] <- downloadHandler(

    filename = paste0(params %()% chosen_protein, "-", input[["diff_viewer_chain"]], ".csv"),
    content = function(file){
                write.csv(woods_test_results(),
                file = file,
                quote = F,
                row.names = F)}
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
