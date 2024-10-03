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
      names = c("variable", "calculation", "state" , "time"),
      modes = list(
        time = "SINGLE POINT",
        state = "SINGLE" ## add option
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

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- reactive({
      # variable <- cov_variable()

      if (grepl("diff", cov_variable())){
        HaDeX::calculate_diff_uptake(
          dat = dat(),
          time_t = s_time %()% t
        )
      } else if (cov_variable() == "auc") {
        dat_tmp <- dat()[dat()[["Exposure"]] < MAX_TIME, ]
        HaDeX::calculate_auc(
          HaDeX::create_uptake_dataset(dat_tmp),
          preserve_values = FALSE
        )
      } else if (cov_variable() == "back_exchange") {
        dat_tmp <- dat()[dat()[["Exposure"]] < MAX_TIME, ]
        HaDeX::calculate_back_exchange(
          dat_tmp,
          states = s_state[["state"]]()
        )
      } else {
        HaDeX::calculate_state_uptake(
          dat = dat(),
          state = s_state[["state"]](),
          time_t = s_time %()% t
        )
      }
    })

    ### GET VARIABLE

    cov_variable <- reactive({

      get_coverage_heatmap_variable(
        type = s_variable %()% variable,
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]]()
      )

    })

    ### OUT REACTIVES

    plot_out <- reactive({

      HaDeX::plot_coverage_heatmap(
        x_dat = dat_processed(),
        value = cov_variable()
      ) %>%
        suppressMessages()
    })

    dat_out <- reactive({

      ## TODO select and round
      dat_processed()

    })

    ### SERVER AND PLOT SETTINGS INVOCATION

    s_calculation <- list({
      theoretical = reactive({ FALSE })
      fractional = reactive({ FALSE })
    })

    invoke_settings_servers(
      names = c("variable", "time", "calculation", "state"),
      modes = c(
        time = "SINGLE POINT",
        state = "SINGLE"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn()
    )
  })
}

