#' plot_comparison_and_woods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_comparison_and_woods_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Comparison and Woods", differential = FALSE),

    settings = hadex_panel_settings(
      hadex_settings_separator("Common settings"),
      mod_settings_calculation_ui(ns("calculation")),
      mod_settings_time_ui(ns("time"), mode = "limits and exposure"),

      hadex_settings_separator("Comparison Plot settings"),
      mod_settings_state_ui(ns("state_comparison"), mode = "multiple"),
      mod_settings_color_ui(ns("color")),

      hadex_settings_separator("Woods Plot settings"),
      mod_settings_state_ui(ns("state_woods"), mode = "double"),
      mod_settings_test_ui(ns("test"), mode = "selectible"),
      mod_settings_visualization_ui(ns("visualization"), mode = "woods"),

      hadex_settings_separator("Adjustment settings"),
      mod_settings_range_ui(
        id = ns("range"),
        range_labs = c(
          comparison_y = construct_auto_range_lab("Comparison", "y"),
          woods_y = construct_auto_range_lab("Woods", "y"),
          x = "Choose x range for both plots:"
        )
      ),
      mod_settings_label_ui(
        id = ns("label"),
        label_labs = construct_auto_label_labs(c("Comparison", "Woods"))
      )
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = c(
        comparison = construct_plot_label("Comparison", differential = FALSE),
        woods = construct_plot_label("Woods", differential = FALSE)
      ),
      additional_data_info = c(
        comparison = cosntruct_uptake_plots_data_info(differential = FALSE),
        woods = cosntruct_uptake_plots_data_info(differential = TRUE)
      )
    )
  )
}

#' plot_comparison_and_woods Server Functions
#'
#' @noRd
mod_plot_comparison_and_woods_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dat_processed_comparison <- reactive({
      states <- s_state_comparison %()% states # calculated outside to handle validation error correctly

      HaDeX::create_uptake_dataset(
        dat(),
        protein   = params %()% chosen_protein,
        states    = states,
        time_0    = s_time %()% 0,
        time_100  = s_time %()% 100,
        deut_part = params %()% deut_part
      ) %>%
        filter(Exposure %in% (s_time %()% t))
    })

    dat_processed_woods <- reactive({
      # TODO bypass this legacy
      validate(need(length(unique(filter(dat(), !is.na("Modification"), Protein == params[["chosen_protein"]]())[["State"]])) > 1,
                    "Not sufficient number of states without modifications."))

      dat() %>%
        HaDeX::create_diff_uptake_dataset(
          protein   = params        %()% chosen_protein,
          state_1   = s_state_woods %()% state_1,
          state_2   = s_state_woods %()% state_2,
          time_0    = s_time        %()% 0,
          time_100  = s_time        %()% 100,
          deut_part = params        %()% deut_part
        ) %>%
        filter(Exposure %in% (s_time %()% t))
    })

    plot_out_comparison <- reactive({
      (dat_processed_comparison() %>%
          HaDeX::plot_state_comparison(
            theoretical = s_calculation %()% theoretical,
            fractional  = s_calculation %()% fractional,
            time_t      = s_time        %()% t,
            all_times   = s_time        %()% multiple
          ) +
          scale_color_manual(values = s_color %()% values)
      ) %>%
        update_axes_and_labels(s_range[["x"]], s_range[["comparison_y"]],
                               s_label, label_prefix = "comparison") %>%
        suppressMessages()
    })

    plot_out_woods <- reactive({
      (dat() %>%
         HaDeX::create_p_diff_uptake_dataset(
           diff_uptake_dat     = dat_processed_woods(),
           protein             = params        %()% chosen_protein,
           state_1             = s_state_woods %()% state_1,
           state_2             = s_state_woods %()% state_2,
           p_adjustment_method = s_test        %()% p_adjustment_method,
           confidence_level    = s_test        %()% confidence_level,
           time_0              = s_time        %()% 0,
           time_100            = s_time        %()% 100,
           deut_part           = params        %()% deut_part
         ) %>%
         HaDeX::plot_differential(
           diff_uptake_dat          = dat_processed_woods(),
           diff_p_uptake_dat        = .,
           theoretical              = s_calculation %()% theoretical,
           fractional               = s_calculation %()% fractional,
           show_houde_interval      = s_test        %()% show_houde,
           hide_houde_insignificant = (s_visualization %()% hide_insignificant) && (s_test %()% show_houde),
           show_tstud_confidence    = s_test        %()% show_tstud,
           hide_tstud_insignificant = (s_visualization %()% hide_insignificant) && (s_test %()% show_tstud),
           time_t                   = s_time        %()% t,
           line_size                = 1,
           confidence_level         = s_test        %()% confidence_level,
           all_times                = s_time        %()% multiple
         )
      ) %>%
        update_axes_and_labels(s_range[["x"]], s_range[["woods_y"]],
                               s_label, label_prefix = "woods") %>%
        suppressMessages()
    })

    dat_out_comparison <- reactive({
      dat_processed_comparison() %>%
        HaDeX::show_uptake_data(
          uptake_dat  = dat_processed_comparison(),
          theoretical = s_calculation %()% theoretical,
          fractional  = s_calculation %()% fractional
        ) %>%
        filter(Protein == params %()% chosen_protein,
               Start >= (s_range %()% x) [[1]],
               End   <= (s_range %()% x) [[2]])
    })

    dat_out_woods <- reactive({
      dat_processed_woods() %>%
        HaDeX::show_diff_uptake_data_confidence(
          theoretical      = s_calculation %()% theoretical,
          fractional       = s_calculation %()% fractional,
          confidence_level = s_test        %()% confidence_level
        ) %>%
        filter(Protein == params %()% chosen_protein,
               Start >= (s_range %()% x) [[1]],
               End   <= (s_range %()% x) [[2]])
    })

    range_specs <- list(
      x = range_spec({
        list(
          min = 1,
          max = params %()% max_range
        )
      }),
      comparison_y = range_spec({
        # TODO: copypasted comment: this should be dynamic as well
        if (s_calculation %()% fractional) list(
          max = 200,
          min = -200,
          value = c(0, 120),
          step = 10
        ) else {
          wait_for(nrow(dat_processed_comparison()) > 0)

          min_abs <- round_any(min(dat_processed_comparison()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, floor)
          max_abs <- round_any(max(dat_processed_comparison()[c("deut_uptake", "theo_deut_uptake")], na.rm = TRUE), 5, ceiling)

          list(
            min = min_abs - 5,
            max = max_abs + 5,
            value = c(min_abs, max_abs),
            step = 1
          )
        }
      }),
      woods_y = range_spec({
        if (s_calculation %()% fractional) list(
          min = -200,
          max = 200,
          value = c(0, 120),
          step = 10
        ) else {
          wait_for(nrow(dat_processed_woods()) > 0)

          min_abs <- round_any(min(dat_processed_woods()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 2, floor)
          max_abs <- round_any(max(dat_processed_woods()[c("diff_deut_uptake", "diff_theo_deut_uptake")], na.rm = TRUE), 2, ceiling)

          list(
            min = min_abs - 2,
            max = max_abs + 2,
            value = c(min_abs, max_abs),
            step = 0.5
          )
        }
      })
    )

    label_specs <- list(
      comparison_title = label_spec(react_construct_uptake_title("deuterium uptake", include_state = FALSE, include_exposure = TRUE)),
      comparison_y = label_spec(react_construct_uptake_lab_y(differential = FALSE)),
      comparison_x = label_spec("Position in sequence"),
      woods_title = label_spec(react_construct_uptake_title("deuterium uptake difference", include_state = FALSE, include_exposure = TRUE)),
      woods_y = label_spec(react_construct_uptake_lab_y(differential = TRUE)),
      woods_x = label_spec("Position in sequence")
    )

    ### run settings servers

    invoke_settings_servers(
      names = c("calculation", "time", "test", "visualization", "range", "label"),
      modes = list(
        time = "limits and exposure",
        test = "selectible",
        visualization = "woods"
      )
    )

    s_state_comparison <- mod_settings_state_server(
      id = "state_comparison",
      mode = "multiple",
      p_states_chosen_protein = params[["states_chosen_protein"]]
    )

    s_state_woods <- mod_settings_state_server(
      id = "state_woods",
      mode = "double",
      p_states_chosen_protein = params[["states_chosen_protein"]]
    )

    s_color <- mod_settings_color_server(
      id = "color",
      s_state = s_state_comparison
    )

    mod_display_plot_server(
      id = "display_plot",
      plot_out = list(
        comparison = plot_out_comparison,
        woods = plot_out_woods
      ),
      dat_out = list(
        comparison = dat_out_comparison,
        woods = dat_out_woods
      )
    )
  })
}
