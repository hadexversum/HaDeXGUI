#' plot_volcano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_volcano_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = construct_plot_label("Volcano", differential = FALSE),

    settings = install_settings_ui(
      names = c("calculation", "state", "time",
                "test", "subregion",
                "visualization", "range", "label"),
      modes = list(
        calculation = "ONLY FRAC",
        state = "DOUBLE",
        time = "LIMITS AND POINTS",
        test = "FIXED",
        visualization = "VOLCANO"
      ),
      params = list(
        range_labs = construct_auto_range_labs("Volcano"),
        label_labs = construct_auto_label_labs("Volcano")
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Volcano", differential = FALSE),
      additional_data_info = cosntruct_uptake_plots_data_info(differential = FALSE),
      additional_plot_info = TRUE
    )
  )
}

#' plot_volcano Server Functions
#'
#' @noRd
mod_plot_volcano_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # TODO: ask why here ticking off `fractional` hides also deut 0%

    ### reactives for data processing

    dat_processed <- reactive({
      validate(need(!params[["has_modifications"]](),
                    "This plot is not yet adjusted for proteins with modifications."))
      wait_for((s_state %()% state_1) != (s_state %()% state_2)) # TODO: find out why is it needed

      dat() %>%
        filter(Protein == (params %()% chosen_protein)) %>% # TODO: is this line necessary?
        HaDeX::create_p_diff_uptake_dataset(
          state_1             = s_state      %()% state_1,
          state_2             = s_state      %()% state_2,
          protein             = params       %()% chosen_protein,
          time_0              = s_time       %()% 0,
          time_100            = s_time       %()% 100,
          deut_part           = params       %()% deut_part,
          p_adjustment_method = s_test       %()% p_adjustment_method ,
          confidence_level    = s_test       %()% confidence_level
        )
    })

    dat_filtered <- reactive({
      dat_processed() %>%
        filter(Exposure %in% (s_time %()% points)) %>%
        filter(Start >= (s_subregion %()% start),
                 End <= (s_subregion %()% end))
    })

    dat_span <- reactive({
      lapply(params %()% times_t, function(t){
        dat() %>%
          HaDeX::calculate_diff_uptake(
            states    = c(s_state %()% state_1, s_state %()% state_2),
            protein   = params %()% chosen_protein,
            time_0    = first(params %()% times), # TODO: should those times be selected like this?
            time_t    = t,
            time_100  = last(params %()% times),
            deut_part = params %()% deut_part
          ) %>%
          mutate(Exposure = t)
      }) %>%
        bind_rows()
    })

    dat_selected <- reactive({
      if ((s_visualization %()% shown_interval) == "Selected time points"){
        dat_span() %>%
          filter(Exposure %in% (s_time %()% points))
      } else
        dat_span()
    })

    houde_intervals <- reactive({
      dat_selected() %>%
        HaDeX::calculate_confidence_limit_values(
          confidence_level = s_test %()% confidence_level,
          theoretical = FALSE,
          fractional = s_calculation %()% fractional
        )
    })

    alpha_interval <- reactive({
      -log(1 - s_test %()% confidence_level)
    })

    dat_out <- reactive({
      HaDeX::show_volcano_data(
        dat_filtered(),
        D_diff_threshold = houde_intervals()[2],
        log_P_threshold = alpha_interval(),
        confidence_level = s_test %()% vol_confidence_level,
        fractional = s_calculation %()% fractional
      )
    })

    plot_out <- reactive({
      range_y <- s_range %()% y
      range_x <- s_range %()% x

      (dat_filtered() %>%
          HaDeX::plot_volcano(
            state_1                 = s_state         %()% state_1,
            state_2                 = s_state         %()% state_2,
            color_times             = s_visualization %()% distinguish_timepoints,
            show_insignificant_grey = s_visualization %()% show_insignificant_grey,
            hide_insignificant      = s_visualization %()% hide_insignificant,
            fractional              = s_calculation   %()% fractional,
            theoretical             = FALSE ## hard coded, no theoretical
          ) +
          # ## statistics
          geom_segment(
            aes(x = houde_intervals()[1],
                xend = houde_intervals()[1],
                y = alpha_interval(),
                yend = range_y[2]),
            linetype = "dashed", color = "red") +
          geom_segment(
            aes(x = houde_intervals()[2],
                xend = houde_intervals()[2],
                y = alpha_interval(),
                yend = range_y[2]),
            linetype = "dashed", color = "red") +
          geom_segment(
            aes(y = alpha_interval(),
                yend = alpha_interval(),
                x = range_x[1],
                xend = houde_intervals()[1]
            ), linetype = "dashed", color = "red") +
          geom_segment(
            aes(y = alpha_interval(),
                yend = alpha_interval(),
                x = houde_intervals()[2],
                xend = range_x[2]
            ), linetype = "dashed", color = "red")
      ) %>%
        update_axes_and_labels(s_range[["x"]], s_range[["y"]], s_label) %>%
        suppressMessages()
    })

    info_out <- reactive({
      glue::glue(
        "Based on the chosen criteria, the threshold of -log(P value) is ",
        "{round(alpha_interval(), 4)}.\nThe threshold on deuterium uptake difference ",
        "is {round(houde_intervals()[1], 4)} and {round(houde_intervals()[2], 4)} ",
        "{if (s_calculation %()% fractional) '[%]' else '[Da]'}."
      )
    })

    ### reactives for settings servers

    range_specs <- list(
      x = range_spec({
        wait_for(nrow(dat_processed()) > 0)

        ntheo <- dat_processed()[[
          construct_var_name(
            diff = TRUE,
            theo = FALSE,
            s_calculation %()% fractional,
            "deut_uptake"
          )
        ]]

        max_abs <- ceiling(max(abs(ntheo), na.rm = TRUE)) ## TODO ?

        list(
          min = -max_abs - 2,
          max = max_abs + 2,
          value = c(-max_abs, max_abs)
        )
      }),
      y = range_spec({
        wait_for(nrow(dat_processed()) > 0)

        max_y <- ceiling(max(dat_processed()[["log_p_value"]], na.rm = TRUE))

        list(
          min = 0,
          max = max_y + 2,
          value = c(0, max_y)
        )
      })
    )

    label_specs <- list(
      title = label_spec(react_construct_uptake_title("volcano", differential = TRUE)),
      x = label_spec(react_construct_uptake_lab_y(differential = TRUE)),
      y = label_spec("-log(P value)")
    )

    ### settings servers

    invoke_settings_servers(
      names = c(
        "calculation", "state", "time",
        "test", "subregion",
        "visualization", "range", "label"
      ),
      modes = list(
        calculation = "ONLY FRAC",
        state = "DOUBLE",
        time = "LIMITS AND POINTS",
        test = "FIXED",
        visualization = "VOLCANO"
      )
    )

    mod_display_plot_server("display_plot", plot_out, dat_out, info_out = info_out)

    return(
      autoreturn()
    )
  })
}
