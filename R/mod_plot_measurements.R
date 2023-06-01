#' plot_measurements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_measurements_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = "Measurements",

    settings = install_settings_ui(
      names = c("state", "time", "peptide", "visualization", "range", "label"),
      modes = c(
        state = "SINGLE",
        time = "SINGLE POINT",
        peptide = "SINGLE",
        visualization = "MEASUREMENTS"
      ),
      params = list(
        range_labs = construct_auto_range_labs("Mass Uptake", axes = "y"),
        label_labs = construct_auto_label_labs(c("Measurements", "Mass Uptake"))
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = c(
        measurements = "Plot",
        mass_uptake = "Plot"
      )
    )
  )
}

#' plot_measurements Server Functions
#'
#' @noRd
mod_plot_measurements_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    peptide_table <- reactive({
      state <- s_state %()% state
      dat() %>%
        filter(Protein == params %()% chosen_protein,
               State == state)  %>%
        select(Sequence, Start, End) %>%
        unique(.) %>%
        arrange(Start, End)
    })

    selected_sequence <- reactive({
      peptide_table()[s_peptide %()% selected, 1]
    })

    dat_processed_measurements <- reactive({
      dat() %>%
        filter(Exposure < MAX_TIME)
    })

    ### OUT REACTIVES

    dat_processed_mass_uptake <- reactive({
      func <- if (s_visualization %()% show_replicates) function(data) {
        HaDeX::calculate_exp_masses_per_replicate(data) %>%
          mutate(mass_uptake = avg_exp_mass - MHP)
      } else function(data) {
        mutate(
          data,
          exp_mass = Center * z - z * PROTON_MASS,
          weighted_Inten = scale(Inten),
          mass_uptake = exp_mass - MHP
        )
      }

      sequence <- selected_sequence()
      state <- s_state %()% state

      dat_processed_measurements() %>%
        filter(
          Protein  == params  %()% chosen_protein,
          State    == state,
          Sequence == selected_sequence(),
          Exposure >  params  %()% no_deut_control
        ) %>%
        func()
    })

    plot_out_measurements <- reactive({
      sequence <- selected_sequence()
      dat_processed_measurements() %>%
        HaDeX::plot_peptide_mass_measurement(
          protein            = params            %()% chosen_protein,
          show_charge_values = !(s_visualization %()% show_replicates),
          state              = s_state           %()% state,
          sequence           = sequence,
          time_t             = s_time            %()% t
        ) %>%
        update_axes_and_labels(labels = s_label, label_prefix = "measurements") %>%
        suppressMessages()
    })

    plot_out_mass_uptake <- reactive({
      sequence = selected_sequence()
      dat_processed_measurements() %>% # TODO: is this sic! or sick?
      HaDeX::plot_replicate_mass_uptake(
        protein         = params          %()% chosen_protein,
        state           = s_state         %()% state,
        sequence        = sequence,
        log_x           = s_visualization %()% log_x,
        aggregated      = s_visualization %()% show_replicates
      ) %>%
        update_axes_and_labels(range_y = s_range[["y"]], labels = s_label, label_prefix = "mass_uptake") %>%
        suppressMessages()
    })

    dat_out_measurements <- reactive({

      sequence <- selected_sequence()
      dat_processed_measurements() %>%
        HaDeX::calculate_exp_masses_per_replicate() %>%
        HaDeX::show_peptide_mass_measurement(
          protein  = params  %()% chosen_protein,
          state    = s_state %()% state,
          sequence = sequence,
          time_t   = s_time  %()% t
        )
    })

    dat_out_mass_uptake <- reactive({
      if (s_visualization %()% show_replicates) {
        dat_processed_mass_uptake() %>%
          mutate(avg_exp_mass = round(avg_exp_mass, 4),
                 mass_uptake = round(mass_uptake, 4)) %>%
          rename("Avg Exp Mass" = avg_exp_mass,
                 "Mass Uptake" = mass_uptake) %>%
          arrange(Start, End)
      } else {
        dat_processed_mass_uptake() %>%
          select(-MaxUptake, -Inten, -Center) %>%
          mutate(exp_mass = round(exp_mass, 4),
                 weighted_Inten = round(weighted_Inten, 4),
                 mass_uptake = round(mass_uptake, 4)) %>%
          rename("Exp Mass" = exp_mass,
                 "Rel Inten" = weighted_Inten,
                 "Mass Uptake" = mass_uptake) %>%
          arrange(Start, End)
      }
    })

    ### VALUES FOR RANGE AND LABEL SERVERS

    range_specs <- list(
      y = range_spec({
        wait_for(nrow(dat_processed_mass_uptake()) > 0)

        max_abs <- ceiling(max(dat_processed_mass_uptake()["mass_uptake"], na.rm = TRUE))

        list(
          min = 0,
          max = max_abs + 5,
          value = c(0, max_abs)
        )
      })
    )

    label_specs <- list(
      measurements_title = label_spec(reactive({glue::glue("Measurements of {selected_sequence()} in {s_time %()% t} min in {s_state %()% state} state")})),
      measurements_y = label_spec(""),
      measurements_x = label_spec("Measured mass [Da]"),
      mass_uptake_title = label_spec(reactive({glue::glue("Mass uptake of {selected_sequence()} in {s_state %()% state} state")})),
      mass_uptake_y = label_spec("Mass [Da]"),
      mass_uptake_x = label_spec("Time point [min]")
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    # Assigning null because time server require this parameter
    # even though it is unused in this mode;
    # TODO: find a workaroud
    s_calculation <- NULL

    invoke_settings_servers(
      names = c("state", "time", "peptide", "visualization", "range", "label"),
      modes = c(
        state = "SINGLE",
        time = "SINGLE POINT",
        peptide = "SINGLE",
        visualization = "MEASUREMENTS"
      )
    )

    mod_display_plot_server(
      id = "display_plot",
      plot_out = list(
        measurements = plot_out_measurements,
        mass_uptake = plot_out_mass_uptake
      ),
      dat_out = list(
        measurements = dat_out_measurements,
        mass_uptake = dat_out_mass_uptake
      )
    )

    ### RETURN OF THE PLOT AND DATA

    return(
      c(
        autoreturn("measurements", "mass_uptake"),
        # additionally return flag
        list(report_validation_peptide_selected = s_peptide[["selected_flag"]])
      )
    )
  })
}
