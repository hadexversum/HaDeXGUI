#' plot_structure_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_structure_heatmap_ui <- function(id, differential){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Hires", differential),

    settings = install_settings_ui(
      names = c("state", "time", "visualization", "range", "label"),
      modes = c(
        state = if (differential) "DOUBLE" else "SINGLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        visualization = "HEATMAP"
      ),
      params = list(
        range_labs = construct_auto_range_labs("Heatmap sequence", axes = "y"),
        label_labs = construct_auto_label_labs("Heatmap")
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_labels = c(
        measurements = "Heatmap Plot"      ),
      additional_data_info = cosntruct_uptake_plots_data_info(differential = T)
    )
  )


}

#' plot_structure_heatmap Server Functions
#'
#' @noRd
mod_plot_structure_heatmap_server <- function(id, differential, dat, str_path, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # structure_out <- reactive({
    #
    #   HRaDeX::plot_3d_structure_blank(pdb_file_path = str_path())
    #
    # })

    dat_out <- reactive({

      dat() %>%
        HaDeX::create_uptake_dataset() %>%
        HaDeX::create_aggregated_uptake_dataset()

    })

    plot_out <- reactive({

      dat() %>%
        HaDeX::create_uptake_dataset(time_0 = 0.001) %>%
        HaDeX::create_aggregated_uptake_dataset() %>%
        HaDeX::plot_aggregated_uptake(panels = F)

    })



    mod_display_plot_server("display_plot", plot_out, dat_out)

    return(
      autoreturn()
    )

  })
}

## To be copied in the UI
# mod_plot_structure_heatmap_ui("plot_structure_heatmap")

## To be copied in the server
# mod_plot_structure_heatmap_server("plot_structure_heatmap")
