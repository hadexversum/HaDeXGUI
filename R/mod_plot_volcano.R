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

  HaDeX_plotTab(
    title = construct_plot_label("Volcano", differential = FALSE, capitalize = TRUE),

    settingsPanel = rlang::exec(
      .fn = HaDeX_plotSettingsPanel,

      !!!install_settings_ui(
        names = c("general", "state", "timepoints",
                  "diff_test", "subregion",
                  "visualization", "range", "labels"),
        params = list(
          differential = TRUE,
          # general:
          theoretical_switch = FALSE,
          # test_diff:
          test_mode = "fixed",
          # visualization:
          uncertainty_mode = "none",
          volcano_switch = TRUE,
          range_ids = c("x", "y"),
          plot_type = "Volcano"
        ),
        ns = ns
      ),
    ),
    displayPanel = mod_display_plot_section_ui(
      ns("display_plot"),
      plot_label = construct_plot_label("Volcano", differential = FALSE),
      additional_data_info = cosntruct_uptake_plots_data_info(differential = FALSE)
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
          time_0              = s_timepoints %()% time_0,
          time_100            = s_timepoints %()% time_100,
          deut_part           = params       %()% deut_part,
          p_adjustment_method = s_diff_test  %()% p_adjustment_method ,
          confidence_level    = s_diff_test  %()% confidence_level
        )
    })

    dat_filtered <- reactive({
      dat_processed() %>%
        filter(Exposure %in% (s_timepoints %()% timepoints)) %>%
        filter(Start >= (s_subregion %()% subregion)[1],
                 End <= (s_subregion %()% subregion)[2])
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
          filter(Exposure %in% (s_timepoints %()% timepoints))
      } else
        dat_span()
    })

    houde_intervals <- reactive({
      dat_selected() %>%
        HaDeX::calculate_confidence_limit_values(
          confidence_level = s_diff_test %()% confidence_level,
          theoretical = FALSE,
          fractional = s_general %()% fractional
        )
    })

    alpha_interval <- reactive({
      -log(1 - s_diff_test %()% confidence_level)
    })

    dat_out <- reactive({
      HaDeX::show_volcano_data(
        dat_filtered(),
        D_diff_threshold = houde_intervals()[2],
        log_P_threshold = alpha_interval(),
        confidence_level = s_diff_test %()% vol_confidence_level,
        fractional = s_general %()% fractional
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
            fractional              = s_general       %()% fractional,
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
        update_axes_and_labels(s_range[["x"]], s_range[["y"]], s_labels) %>%
        suppressMessages()
    })

    ### reactives for settings servers

    range_specs <- list(
      range_spec({
        wait_for(nrow(dat_processed()) > 0)

        ntheo <- dat_processed()[[
          construct_var_name(
            diff = TRUE,
            theo = FALSE,
            s_general %()% fractional,
            "deut_uptake"
          )
        ]]

        max_abs <- ceiling(max(abs(ntheo), na.rm = TRUE)) ## TODO ?

        list(
          min = -max_abs - 2,
          max = max_abs + 2,
          value = c(-max_abs, max_abs)
        )
      }, "x"),
      range_spec({
        wait_for(nrow(dat_processed()) > 0)

        max_y <- ceiling(max(dat_processed()[["log_p_value"]], na.rm = TRUE))

        list(
          min = 0,
          max = max_y + 2,
          value = c(0, max_y)
        )
      }, "y")
    )

    label_specs <- list(
      label_spec(react_construct_uptake_title("volcano", differential = TRUE), "title"),
      label_spec(react_construct_uptake_lab_y(differential = TRUE), "x"),
      label_spec("-log(P value)", "y")
    )

    ### settings servers

    invoke_settings_servers(
      names = c(
        "general", "state", "timepoints",
        "diff_test", "subregion",
        "visualization", "range", "labels"
      ),
      const_params = list(
        differential = TRUE,
        # general:
        theoretical_switch = FALSE,
        # visualization:
        uncertainty_mode = "none",
        log_x_switch = FALSE,
        volcano_switch = TRUE,
        # diff_test:
        test_mode = "fixed"
      )
    )

    mod_display_plot_section_server("display_plot", plot_out, dat_out)
  })
}
