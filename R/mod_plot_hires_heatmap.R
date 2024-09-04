#' plot_chiclet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_hires_heatmap_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = "Single heatmap",

    settings = install_settings_ui(
      names = c("calculation", "state", "time"),
      modes = c(
        state = "SINGLE",
        time = "ONLY LIMITS"
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = "Heatmap"
    )
  )
}

#' plot_chiclet Server Functions
#'
#' @noRd
mod_plot_hires_heatmap_server <- function(id, dat, params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <-  reactive({

      dat() %>%
        HaDeX::create_uptake_dataset(
          protein = params[["chosen_protein"]](),
          states = s_state[["state"]](),
          time_0 = s_time[["0"]](),
          time_100 = s_time[["100"]]()
        ) %>%
        HaDeX::create_aggregated_uptake_dataset()
    })

    ### OUT REACTIVES

    plot_out <- reactive({
      (dat_processed() %>%
         HaDeX::plot_aggregated_uptake(panels = F,
                                       fractional = s_calculation[["fractional"]](),
                                       theoretical = s_calculation[["theoretical"]]())
      )  %>%
        suppressMessages()
    })

    .show_fun <- HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed()

    })


    label_specs <- list(
      title = label_spec("Test"),
      y = label_spec("Exposure [min]"),
      x = label_spec("Peptide ID")
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    invoke_settings_servers(
      names = c(
        "calculation", "state", "time"
      ),
      modes = c(
        state = "SINGLE"
      )
    )

    ### RETURN OF THE PLOT AND DATA

    mod_display_plot_server("display_plot", plot_out, dat_out)

    return(
      autoreturn()
    )
  })
}
