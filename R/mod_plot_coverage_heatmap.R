#' plot_coverage_heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_coverage_heatmap_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = "Coverage Heatmap Plot",

    settings = install_settings_ui(
      names = c("variable", "time"),
      modes = list(
        time = "SINGLE POINT"
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_labels = "Plot"
    )
  )
}

#' plot_coverage_heatmap Server Functions
#'
#' @noRd
mod_plot_coverage_heatmap_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dat_processed <- reactive({
      variable <- s_variable %()% variable

      if (grepl("diff", variable)){
        HaDeX::calculate_diff_uptake(
          dat = dat(),
          time_t = s_time %()% t
        )
      } else if (variable == "auc") {
        dat_tmp <- dat()[dat()[["Exposure"]] < MAX_TIME, ]
        HaDeX::calculate_auc(
          HaDeX::create_uptake_dataset(dat_tmp),
          preserve_values = FALSE
        )
      } else if (variable == "back_exchange") {
        dat_tmp <- dat()[dat()[["Exposure"]] < MAX_TIME, ]
        HaDeX::calculate_back_exchange(
          dat_tmp,
          states = dat()[["State"]][1]
        )
      } else {
        HaDeX::calculate_state_uptake(
          dat = dat(),
          time_t = s_time %()% t
        )
      }
    })

    plot_out <- reactive({
      HaDeX::plot_coverage_heatmap(
        x_dat = dat_processed(),
        value = s_variable %()% variable
      ) %>%
        suppressMessages()
    })

    dat_out <- reactive({
      dat_processed
    })

    s_calculation <- list({
      theoretical = reactive({ FALSE })
      fractional = reactive({ FALSE })
    })

    invoke_settings_servers(
      names = c("variable", "time"),
      modes = c(
        time = "SINGLE POINT"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)

    return(
      autoreturn()
    )
  })
}

