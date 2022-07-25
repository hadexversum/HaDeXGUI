vis_mode_map <- list(
  UPTAKE = c("log_x", "uncertainty_type"),
  CHICLET = c("show_uncertainty"),
  BUTTERFLY = c("uncertainty_type"),
  VOLCANO = c("shown_interval", "distinguish_timepoints", "hide_insignificant", "show_insignificant_grey"),
  WOODS = c("hide_insignificant"),
  MANHATTAN = c("show_length", "split_timepoints"),
  UNCERTAINTY = c("show_aggregated", "split_timepoints"),
  MEASUREMENTS = c("log_x", "show_replicates"),
  `SEQUENCE DATA` = c("show_residues")
)

#' settings_visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_visualization_ui <- function(id, mode){
  ns <- NS(id)

  vis_settings <- list(
    log_x = checkboxInput_h(
      inputId = ns("log_x"),
      label = "Logarithmic x scale for uptake plot?",
      value = TRUE
    ),
    show_uncertainty = checkboxInput_h(
      inputId = ns("show_uncertainty"),
      label = "Show uncertainty?",
      value = TRUE
    ),
    uncertainty_type = selectizeInput_h(
      inputId = ns("uncertainty_type"),
      label = "Show uncertainty as:",
      choices = c("ribbon", "bars", "bars + line"),
      selected = "ribbon"
    ),
    shown_interval = selectizeInput_h(
      inputId = ns("shown_interval"),
      label = "Show confidence limit for: ",
      choices = c("All time points", "Selected time points"),
      selected = "All time points"
    ),
    distinguish_timepoints = checkboxInput_h(
      inputId = ns("distinguish_timepoints"),
      label = "Distinguish timepoints by color?",
      value = TRUE
    ),
    hide_insignificant = checkboxInput_h(
      inputId = ns("hide_insignificant"),
      label = "Hide insignificant values?",
      value = FALSE
    ),
    show_insignificant_grey = checkboxInput_h(
      inputId = ns("show_insignificant_grey"),
      label = "Show insignificant values in grey?",
      value = FALSE
    ),
    show_length = checkboxInput_h(
      inputId = ns("show_length"),
      label = "Show peptide length and position in the sequence?",
      value = TRUE
    ),
    show_aggregated = checkboxInput_h(
      inputId = ns("show_aggregated"),
      label = "Show aggregated data?",
      value = TRUE
    ),
    split_timepoints = checkboxInput_h(
      inputId = ns("split_timepoints"),
      label = "Show time points separately?",
      value = FALSE
    ),
    show_replicates = checkboxInput_h(
      inputId = ns("show_replicates"),
      label = "Show replicate values?",
      value = FALSE
    ),
    show_residues = selectizeInput(
      inputId = ns("show_residues"),
      label = "Select group of residues to display: ",
      choices = c("Hydrophilic and hydrophobic", "Only hydrophilic", "Only hydrophobic"),
      selected = "Hydrophilic and hydrophobic",
      options = list(dropdownParent = 'body')
    )
  )

  collapsible_card(
    title = "Visualization",
    vis_settings[vis_mode_map[[mode]]],
    fancy_icon = "image"
  )
}

#' settings_visualization Server Functions
#'
#' @noRd
mod_settings_visualization_server <- function(id, mode){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      input_r_list(vis_mode_map[[mode]])
    )
  })
}
