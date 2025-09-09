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
        state = "SINGLE",
        time = "LIMITS AND POINTS",
        visualization = "UNCERTAINTY"
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

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- reactive({
      times <- s_time %()% points

      dat() %>%
        filter(Exposure %in% times)
    })

    ### OUT REACTIVES

    plot_out <- reactive({

      HaDeX::plot_uncertainty(
        dat            = dat_processed(),
        skip_amino     = s_visualization %()% skip_amino,
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

    ### SERVER AND PLOT SETTINGS INVOCATION

    # Assigning null because time server require this parameter
    # even though it is unused in this mode;
    # TODO: find a workaroud
    s_calculation <- list(
      fractional = reactive({ FALSE }),
      theoretical = reactive({ TRUE })
    )

    invoke_settings_servers(
      names = c("state", "time", "visualization"),
      modes = list(
        state = "SINGLE",
        time = "LIMITS AND POINTS",
        visualization = "UNCERTAINTY"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out, plot_name = "uncertainty_plot")

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn()
    )
  })
}

