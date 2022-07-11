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
  HaDeX_plotTab(
    title = construct_plot_label("Butterfly", differential, capitalize = TRUE),

    settingsPanel = rlang::exec(
      .fn = HaDeX_plotSettingsPanel,

      !!!install_settings_ui(
        names = c("general", "state", "timepoints", "diff_test", "visualization", "range", "labels"),
        params = list(
          differential = differential,
          uncertainty_mode = "select",
          range_ids = c("x", "y"),
          plot_type = "Butterfly"
        ),
        ns = ns
      )
    ),
    displayPanel = mod_display_plot_section_ui(
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

    dat_processed <- if (differential) reactive({
      # TODO: check which validates are really needed
      wait_for(s_timepoints[["timepoints"]]())

      HaDeX::create_diff_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state_1 = s_state[["state_1"]](),
        state_2 = s_state[["state_2"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]]() %||% max(dat()[["Exposure"]]),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_timepoints[["timepoints"]]())
    }) else reactive({
      HaDeX::create_state_uptake_dataset(
        dat(),
        protein = params[["chosen_protein"]](),
        state = s_state[["state"]](),
        time_0 = s_timepoints[["time_0"]](),
        time_100 = s_timepoints[["time_100"]](),
        deut_part = params[["deut_part"]]()
      ) %>%
        filter(Exposure %in% s_timepoints[["timepoints"]]())
    })

    plot_out <- if (differential) reactive({
      (dat() %>%
         HaDeX::create_p_diff_uptake_dataset(
           diff_uptake_dat = dat_processed(),
           protein = params[["chosen_protein"]](),
           state_1 = s_state[["state_1"]](),
           state_2 = s_state[["state_2"]](),
           confidence_level = s_diff_test[["confidence_level"]](),
           p_adjustment_method = s_diff_test[["p_adjustment_method"]](),
           time_0 = s_timepoints[["time_0"]](),
           time_100 = s_timepoints[["time_100"]](),
           deut_part = params[["deut_part"]]()
         ) %>% HaDeX::plot_differential_butterfly(
           diff_uptake_dat = dat_processed(),
           diff_p_uptake_dat = .,
           theoretical = s_general[["theoretical"]](),
           fractional = s_general[["fractional"]](),
           uncertainty_type = s_visualization[["uncertainty_type"]](),
           show_houde_interval = s_diff_test[["show_houde"]](),
           show_tstud_confidence = s_diff_test[["show_tstud"]](),
           confidence_level = s_diff_test[["confidence_level"]]()
         ) +
         geom_point_interactive(
           data = dat_processed(),
           aes(x = ID, y = 10, tooltip = paste0( #PLACEHOLDER
             Sequence,
             "<br/>Position: ", Start, "-", End,
             "<br/>Value: PLACEHOLDER", #TODO: find a way of obtaining this value seamlessly
             "<br/>Exposure: ", Exposure, " min"
           ))
         )
      ) %>% update_axes_and_labels(s_range[["x"]], s_range[["y"]], s_labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    }) else reactive({
      validate(need(s_timepoints[["timepoints"]](),
                    "Wait for parameters to be loaded"))

      (dat_processed() %>%
        HaDeX::plot_butterfly(
          theoretical = s_general[["theoretical"]](),
          fractional = s_general[["fractional"]](),
          uncertainty_type = s_visualization[["uncertainty_type"]]()
        ) +
          geom_point_interactive( #TODO: fix this redundancy?
            aes(tooltip = paste0(
              Sequence,
              "<br/>Position: ", Start, "-", End,
              "<br/>Value: ", round(value, 2),
              "<br/>Exposure: ", Exposure, " min"
            ))
          )
      ) %>% update_axes_and_labels(s_range[["x"]], s_range[["y"]], s_labels) %>%
        suppressMessages() # suppressing annoying coordinate system replacement msg
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed() %>%
        .show_fun(
          theoretical = s_general[["theoretical"]](),
          fractional = s_general[["fractional"]]()
        ) %>%
        filter(ID >= s_range[["x"]]()[[1]] &
               ID <= s_range[["x"]]()[[2]])
    })

    ### server reactives

    range_specs <- list(
      range_spec({
        wait_for(nrow(dat_processed()) > 0)

        list(
          min = min(dat_processed()[["ID"]]),
          max = max(dat_processed()[["ID"]])
        )
      }, id = "x"),
      range_spec({
        wait_for(nrow(dat_processed()) > 0)

        theo <- dat_processed()[[
          construct_var_name(
            differential,
            TRUE,
            s_general[["fractional"]](),
            "deut_uptake"
          )
        ]]
        ntheo <- dat_processed()[[
          construct_var_name(
            differential,
            FALSE,
            s_general[["fractional"]](),
            "deut_uptake"
          )
        ]]

        list(
          min = floor(min(theo, ntheo, na.rm = TRUE)) - 1,
          max = ceiling(max(theo, ntheo, na.rm = TRUE)) + 1
        )
      }, id = "y")
    )

    label_specs <- list(
      label_spec(react_construct_uptake_title("butterfly", differential), "title"),
      label_spec("Peptide ID", "x"),
      label_spec(react_construct_uptake_lab_y(differential), "y")
    )

    ### server settings

    invoke_settings_servers(
      names = c(
        "general", "state", "timepoints", "visualization",
        "range", "labels", "diff_test"
      ),
      const_params = list(
        uncertainty_mode = "select",
        log_x_switch = FALSE,
        theoretical_switch = TRUE
      )
    )

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
