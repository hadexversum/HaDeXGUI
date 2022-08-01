#' plot_replicates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_replicates_ui <- function(id){
  ns <- NS(id)
  hadex_tab_plot(
    title = "Replicates",

    settings = install_settings_ui(
      names = c("state", "time"),
      modes = list(
        state = "SINGLE",
        time = "SINGLE POINT"
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_labels = c(
        per_peptide_for_time = "Plot",
        per_peptide_all_times = "Plot",
        per_time = "Plot"
      )
    )
  )
}

#' plot_replicates Server Functions
#'
#' @noRd
mod_plot_replicates_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed_per_peptide_for_time <- reactive({
      HaDeX::create_replicate_dataset(
        dat(),
        time_t  = s_time  %()% t,
        protein = params  %()% chosen_protein,
        state   = s_state %()% state
      )
    })

    dat_processed_per_peptide_all_times <- reactive({
      HaDeX::create_replicate_dataset(
        dat(),
        protein = params  %()% chosen_protein,
        state   = s_state %()% state
      )
    })

    ### OUT REACTIVES

    dat_out_per_peptide_for_time <- reactive({
      HaDeX::show_replicate_histogram_data(dat_processed_per_peptide_for_time())
    })

    dat_out_per_peptide_all_times <- reactive({
      HaDeX::show_replicate_histogram_data(dat_processed_per_peptide_all_times())
    })

    dat_out_per_time <- reactive({
      HaDeX::show_replicate_histogram_data(dat_processed_per_peptide_all_times()) %>%
        arrange(Exposure, ID) %>%
        select(Exposure, everything())
    })

    plot_out_per_peptide_for_time <- reactive({
      HaDeX::plot_replicate_histogram(dat_processed_per_peptide_for_time())
    })

    plot_out_per_peptide_all_times <- reactive({
      HaDeX::plot_replicate_histogram(dat_processed_per_peptide_all_times())
    })

    plot_out_per_time <- reactive({
      HaDeX::plot_replicate_histogram(
        dat_processed_per_peptide_all_times(),
        time_points = TRUE
      )
    })

    ### SERVER AND PLOT SETTINGS INVOCATION

    # Assigning null because time server require this parameter
    # even though it is unused in this mode;
    # TODO: find a workaroud
    s_calculation <- NULL

    invoke_settings_servers(
      names = c("state", "time"),
      modes = list(
        state = "SINGLE",
        time = "SINGLE POINT"
      )
    )

    mod_display_plot_server(
      id = "display_plot",
      plot_out = list(
        per_peptide_for_time = plot_out_per_peptide_for_time,
        per_peptide_all_times = plot_out_per_peptide_all_times,
        per_time = plot_out_per_time
      ),
      dat_out = list(
        per_peptide_for_time = dat_out_per_peptide_for_time,
        per_peptide_all_times = dat_out_per_peptide_all_times,
        per_time = dat_out_per_time
      )
    )

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn("per_peptide_for_time", "per_peptide_all_times", "per_time")
    )
  })
}
