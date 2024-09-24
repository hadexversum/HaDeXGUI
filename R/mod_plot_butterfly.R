#' plot_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_plot_butterfly_ui <- function(id, differential) {
  ns <- NS(id)
  hadex_tab_plot(
    title = construct_plot_label("Butterfly", differential),

    settings = install_settings_ui(
      names = c("calculation", "state", "time", "test", "visualization", "label"), ## "range" deleted
      modes = list(
        state = if (differential) "DOUBLE" else "SINGLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        visualization = "BUTTERFLY"
      ),
      params = list(
        # range_labs = construct_auto_range_labs("Butterfly", differential = differential),
        label_labs = construct_auto_label_labs("Butterfly", differential = differential)
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Butterfly", differential),
      additional_data_info = cosntruct_uptake_plots_data_info(differential)
    )
  )
}

#' plot_butterfly Server Functions
#'
#' @import ggplot2
#' @noRd
mod_plot_butterfly_server <- function(id, differential, dat, params){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- if (differential) reactive({
      wait_for(s_time[["points"]]())

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
      HaDeX::create_state_uptake_dataset(
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
         ) %>% HaDeX::plot_differential_butterfly(
           diff_uptake_dat = dat_processed(),
           diff_p_uptake_dat = .,
           theoretical = s_calculation[["theoretical"]](),
           fractional = s_calculation[["fractional"]](),
           uncertainty_type = s_visualization[["uncertainty_type"]](),
           show_houde_interval = s_test[["show_houde"]](),
           show_tstud_confidence = s_test[["show_tstud"]](),
           confidence_level = s_test[["confidence_level"]](),
           interactive = TRUE
         )
      ) %>% update_axes_and_labels(labels = s_label) %>% ## s_range[["x"]], s_range[["y"]]
        suppressMessages() # suppressing annoying coordinate system replacement msg
    }) else reactive({
      wait_for(s_time %()% points)

      (dat_processed() %>%
        HaDeX::plot_butterfly(
          theoretical = s_calculation[["theoretical"]](),
          fractional = s_calculation[["fractional"]](),
          uncertainty_type = s_visualization[["uncertainty_type"]](),
          interactive = TRUE
        )
      ) %>% update_axes_and_labels(labels = s_label) %>% ## s_range[["x"]], s_range[["y"]],
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
    #   }),
    #   y = range_spec({
    #     wait_for(nrow(dat_processed()) > 0)
    #
    #     theo <- dat_processed()[[
    #       construct_var_name(
    #         differential,
    #         TRUE,
    #         s_calculation[["fractional"]](),
    #         "deut_uptake"
    #       )
    #     ]]
    #     ntheo <- dat_processed()[[
    #       construct_var_name(
    #         differential,
    #         FALSE,
    #         s_calculation[["fractional"]](),
    #         "deut_uptake"
    #       )
    #     ]]
    #
    #     list(
    #       min = floor(min(theo, ntheo, na.rm = TRUE)) - 1,
    #       max = ceiling(max(theo, ntheo, na.rm = TRUE)) + 1
    #     )
    #   })
    # )

    label_specs <- list(
      title = label_spec(react_construct_uptake_title("butterfly", differential)),
      x = label_spec("Peptide ID"),
      y = label_spec(react_construct_uptake_lab_y(differential))
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    invoke_settings_servers(
      names = c("calculation", "state", "time", "test", "visualization", "label"), ## "range"
      modes = list(
        state = if (differential) "DOUBLE" else "SINGLE",
        test = if (differential) "SELECTIBLE" else "DISABLED",
        visualization = "BUTTERFLY"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out)

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn()
    )
  })
}
