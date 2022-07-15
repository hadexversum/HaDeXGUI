#' plot_quality_control UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_quality_control_ui <- function(id){
  ns <- NS(id)
  hadex_tab_plot(
    title = "Quality Control",

    settings = install_settings_ui(
      names = c("state", "time"),
      modes = list(
        state = "double",
        time = "LIMITS AND SINGLE EXPOSURE"
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label ="Quality Control"
    )
  )
}

#' plot_quality_control Server Functions
#'
#' @noRd
mod_plot_quality_control_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dat_processed <- reactive({
      # TODO: restore this check:
      # validate(need(
      #   sum(unique(qc_dat[["Exposure"]]) > as.numeric(input[["qc_time_t"]])) > 1,
      #   "Not enough time points (bigger than chosen time) to generate a plot. "))
      qc_dat <- dat() %>%
        filter(Exposure < MAX_TIME) %>%
        HaDeX::create_quality_control_dataset(
          state_1   = s_state %()% state_1,
          state_2   = s_state %()% state_2,
          time_t    = s_time  %()% t,
          time_0    = s_time  %()% 0,
          protein   = params  %()% chosen_protein,
          deut_part = params  %()% deut_part
        )
    })

    plot_out <- reactive({
      HaDeX::plot_quality_control(dat_processed())
    })

    dat_out <- reactive({
      HaDeX::show_quality_control_data(dat_processed())
    })

    # fix values for calculation
    s_calculation <- list(
      fractional = reactive({ FALSE }), # TODO: ask about it, should there be 100% to choose?
      theoretical = reactive({ FALSE })
    )

    invoke_settings_servers(
      names = c("state", "time"),
      modes = list(
        state = "double",
        time = "LIMITS AND SINGLE EXPOSURE"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)
  })
}

