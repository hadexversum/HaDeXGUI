#' plot_uptake UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_uptake_ui <- function(id, differential) {
  ns <- NS(id)

  HaDeX_plotTab(
    title = construct_plot_label("Uptake Curves", differential, capitalize = TRUE),

    settingsPanel = rlang::exec(
      .fn = HaDeX_plotSettingsPanel,

      !!!install_settings_ui(
        names = c("general", "timepoints", #"peptide",
                  "visualization", "range", "labels"),
        params = list(
          differential = differential,
          uncertainty_switch = "select",
          log_x_switch = TRUE,
          range_ids = c("y"),
          plot_type = "Uptake curves"
        ),
        ns = ns
      )
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Uptake curves", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential)
    )
  )
}

#' plot_uptake Server Functions
#'
#' @noRd
mod_plot_uptake_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_plot_uptake_ui("plot_uptake_1")

## To be copied in the server
# mod_plot_uptake_server("plot_uptake_1")
