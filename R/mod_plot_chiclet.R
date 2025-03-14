#' plot_chiclet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_chiclet_ui <- function(id, differential){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Chiclet", differential),

    settings = install_settings_ui(
      names = c("calculation", "state", "time", "test", "visualization", "label"), # range deleted
      modes = c(
        state = if (differential) "DOUBLE" else "SINGLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        visualization = "CHICLET"
      ),
      params = list(
        # range_labs = construct_auto_range_labs("Chiclet", axes = "x", differential = differential),
        label_labs = construct_auto_label_labs("Chiclet", differential = differential)
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Chiclet", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential)
    )
  )
}

#' plot_chiclet Server Functions
#'
#' @noRd
mod_plot_chiclet_server <- function(id, differential, dat, params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- if (differential) reactive({
      wait_for(s_time %()% points)

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state_1 = s_state[["state_1"]](),
        state_2 = s_state[["state_2"]](),
        time_0 = s_time[["0"]](),
        time_100 = s_time[["100"]](),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_time[["points"]]())
    }) else reactive({
      validate(need(s_time[["points"]](),
                    "Wait for parameters to be loaded"))

      dt <- HaDeX::create_state_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state = s_state[["state"]](),
        time_0 = s_time[["0"]](),
        time_100 = s_time[["100"]](),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_time[["points"]]())
    })

    ### OUT REACTIVES

    plot_out <- if (differential) reactive({
      (dat() %>%
         HaDeX::create_p_diff_uptake_dataset(
           diff_uptake_dat = dat_processed(),
           protein = params[["chosen_protein"]](),
           state_1 = s_state[["state_1"]](),
           state_2 = s_state[["state_2"]](),
           confidence_level = s_test[["confidence_level"]](),
           p_adjustment_method = s_test[["p_adjustment_method"]](),
           time_0 = s_time[["0"]](),
           time_100 = s_time[["100"]](),
           deut_part = params[["deut_part"]]()
         ) %>% HaDeX::plot_differential_chiclet(
           diff_uptake_dat = dat_processed(),
           diff_p_uptake_dat = .,
           theoretical = s_calculation[["theoretical"]](),
           fractional = s_calculation[["fractional"]](),
           show_uncertainty = s_visualization[["show_uncertainty"]](),
           show_houde_interval = s_test[["show_houde"]](),
           show_tstud_confidence = s_test[["show_tstud"]](),
           confidence_level = s_test[["confidence_level"]]()
         )
      ) %>% update_axes_and_labels(labels = s_label) %>% ## range_x = s_range[["x"]],
        suppressMessages() # suppressing annoying coordinate system replacement msg
    }) else reactive({
      (dat_processed() %>%
         HaDeX::plot_chiclet(
           theoretical = s_calculation[["theoretical"]](),
           fractional = s_calculation[["fractional"]](),
           show_uncertainty = s_visualization[["show_uncertainty"]]()
         )
      ) %>% update_axes_and_labels(labels = s_label) %>% ## range_x = s_range[["x"]],
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = s_calculation[["theoretical"]](),
          fractional = s_calculation[["fractional"]]())
        # ) %>%
        # filter(ID >= s_range[["x"]]()[[1]] &
        #        ID <= s_range[["x"]]()[[2]])
    })

    ### VALUES FOR RANGE AND LABEL SERVERS

    # range_specs <- list(
    #   x = range_spec({
    #     wait_for(nrow(dat_processed()) > 0)
    #
    #     list(
    #       min = min(dat_processed()[["ID"]]),
    #       max = max(dat_processed()[["ID"]])
    #     )
    #   })
    # )

    label_specs <- list(
      title = label_spec(react_construct_uptake_title("Chiclet", differential)),
      y = label_spec("Exposure [min]"),
      x = label_spec("Peptide ID")
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    invoke_settings_servers(
      names = c(
        "calculation", "state", "time", "test", "visualization", "label" # "range"
      ),
      modes = c(
        state = if (differential) "DOUBLE" else "SINGLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        visualization = "CHICLET"
      )
    )

    ### RETURN OF THE PLOT AND DATA

    mod_display_plot_server("display_plot", plot_out, dat_out)

    return(
      autoreturn()
    )
  })
}
