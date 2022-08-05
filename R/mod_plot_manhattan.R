#' plot_manhattan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_manhattan_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Manhattan", differential = FALSE),

    settings = install_settings_ui(
      names = c("state", "time", "test", "visualization", "label"),
      modes = list(
        state = "DOUBLE",
        time = "LIMITS AND POINTS",
        test = "FIXED",
        visualization = "MANHATTAN"
      ),
      params = list(
        label_labs = construct_auto_label_labs("Manhattan")
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Manhattan", differential = FALSE),
      additional_data_info = cosntruct_uptake_plots_data_info(differential = FALSE)
    )
  )
}

#' plot_manhattan Server Functions
#'
#' @noRd
mod_plot_manhattan_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- reactive({
      state_1 <- s_state %()% state_1
      state_2 <- s_state %()% state_2
      HaDeX::create_p_diff_uptake_dataset(
        dat = dat(),
        protein             = params  %()% chosen_protein,
        state_1             = state_1,
        state_2             = state_2,
        p_adjustment_method = s_test  %()% p_adjustment_method
      )
    })

    ### OUT REACTIVES

    plot_out <- reactive({
      HaDeX::plot_manhattan(
        p_dat                 = dat_processed(),
        plot_title            = "Difference uptake",
        skip_amino            = s_visualization %()% skip_amino,
        confidence_level      = s_test          %()% confidence_level,
        times                 = s_time          %()% points,
        separate_times        = s_visualization %()% split_timepoints,
        show_confidence_limit = TRUE,
        show_peptide_position = s_visualization %()% show_length
      ) %>%
        update_axes_and_labels(labels = s_label)
    })

    dat_out <- reactive({
      dat_processed() %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, P_value, log_p_value) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               P_value = round(P_value, 4),
               log_p_value = round(log_p_value, 4))
    })

    ### VALUES FOR RANGE AND LABEL SERVERS

    label_specs <- list(
      title = label_spec(react_construct_uptake_title("deuterium uptake difference", differential = TRUE)),
      x = label_spec("Peptide position"),
      y = label_spec("-log(P value)")
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    # fix values for calculation
    s_calculation <- list(
      fractional = reactive({ FALSE }),
      theoretical = reactive({ TRUE })
    )

    invoke_settings_servers(
      names = c("state", "time", "test", "visualization", "label"),
      modes = list(
        state = "DOUBLE",
        time = "LIMITS AND POINTS",
        test = "FIXED",
        visualization = "MANHATTAN"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn()
    )
  })
}
