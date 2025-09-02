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
      names = c("variable", "state", "time"),
      modes = list(
        time = "ONLY LIMITS",
        state = "SINGLE"
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


       if (cov_variable() == "auc") {
        dat_tmp <- dat()[dat()[["Exposure"]] < MAX_TIME, ]
        HaDeX::calculate_auc(
          uptake_dat = HaDeX::create_uptake_dataset(
            dat = dat_tmp,
            protein = params  %()% chosen_protein,
            states = s_state[["state"]](),
            time_0 = s_time  %()% 0,
            time_100 = s_time  %()% 100,
            deut_part = params  %()% deut_part
            ),
          protein = params  %()% chosen_protein,
          state = s_state[["state"]](),
          preserve_values = FALSE
        )
      } else if (cov_variable() == "back_exchange") {
        validate(need(s_time  %()% 100 %in% unique(dat()[dat()[["State"]] == s_state[["state"]](), "Exposure"]), "Selected fully deuterated time point not present for selected state!"))
        HaDeX::calculate_back_exchange(
          dat = dat(),
          protein = params  %()% chosen_protein,
          states = s_state[["state"]](),
          time_100 = s_time  %()% 100
        )
      }
    })

    ### GET VARIABLE

    cov_variable <- reactive({

        get_coverage_heatmap_variable(
        type = s_variable %()% variable,
        theoretical = s_calculation[["theoretical"]](),
        fractional = s_calculation[["fractional"]]())

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

      HaDeX::show_coverage_heatmap_data(x_dat = dat_processed(),
                                 value = cov_variable())

    })

    ### SERVER AND PLOT SETTINGS INVOCATION

    s_calculation <- list(
      theoretical = reactive({ FALSE }),
      fractional = reactive({ TRUE })
    )

    invoke_settings_servers(
      names = c("variable", "state", "time"),
      modes = c(
        time = "ONLY LIMITS",
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

