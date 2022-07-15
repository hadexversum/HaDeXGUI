#' plot_uncertainty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_uncertainty_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Uncertainty"),

    settings = install_settings_ui(
      names = c("state", "time", "visualization"),
      modes = list(
        state = "single",
        time = "limits and points",
        visualization = "uncertainty"
      ),
      params = list(
        label_labs = construct_auto_label_labs("Uncertainty")
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Uncertainty"),
      additional_data_info = cosntruct_uptake_plots_data_info(differential = TRUE)
    )
  )
}

#' plot_uncertainty Server Functions
#'
#' @noRd
mod_plot_uncertainty_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dat_processed <- reactive({
      times <- s_time %()% points

      dat() %>%
        filter(Exposure %in% times)
    })

    plot_out <- reactive({
      HaDeX::plot_uncertainty(
        dat            = dat_processed(),
        protein        = params          %()% chosen_protein,
        state          = s_state         %()% state,
        aggregated     = s_visualization %()% show_aggregated,
        separate_times = s_visualization %()% split_timepoints
      )
    })

    dat_out <- reactive({
      func <- if (s_visualization %()% show_aggregated) {
        HaDeX::calculate_exp_masses
      } else {
        function(dat) {
          dat %>%
            mutate(exp_mass = Center * z - z * PROTON_MASS) %>%
            select(-Inten, -Center,  -MaxUptake, -z) %>%
            group_by(Protein, Sequence, Start, End, MHP, State, Exposure) %>%
            mutate(err_avg_mass = sd(exp_mass)) %>%
            select(-exp_mass) %>%
            unique()
        }
      }
      dat_processed() %>%
        func() %>%
        mutate(avg_mass = round(avg_mass, 4),
               err_avg_mass = round(err_avg_mass, 4))
    })

    # fix values for calculation
    s_calculation <- list(
      fractional = reactive({ FALSE }),
      theoretical = reactive({ TRUE })
    )

    invoke_settings_servers(
      names = c("state", "time", "visualization"),
      modes = list(
        state = "single",
        time = "limits and points",
        visualization = "uncertainty"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)

  })
}
