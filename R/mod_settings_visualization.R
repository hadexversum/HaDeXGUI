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
  collapsible_card(
    title = "Visualization",

    checkboxInput_h(
      inputId = ns("log_x"),
      label = "Logaritmic x scale",
      value = TRUE
    ) %nullify if% !(mode %in% c("UPTAKE")),
    checkboxInput_h(
      inputId = ns("show_uncertainty"),
      label = "Show uncertainty",
      value = TRUE
    ) %nullify if% !(mode %in% c("CHICLET")),
    selectInput_h(
      inputId = ns("uncertainty_type"),
      label = "Show uncertainty as:",
      choices = c("ribbon", "bars", "bars + line"),
      selected = "ribbon"
    ) %nullify if% !(mode %in% c("UPTAKE", "BUTTERFLY")),
    tagList(
      selectInput_h(
        inputId = ns("shown_interval"),
        label = "Show confidence limit for: ",
        choices = c("All time points", "Selected time points"),
        selected = "All time points"
      ),
      checkboxInput_h(
        inputId = ns("distinguish_timepoints"),
        label = "Distinguish timepoints by color",
        value = TRUE
      ),
      checkboxInput_h(
        inputId = ns("hide_insignificant"),
        label = "Hide insignificant values?",
        value = FALSE
      ),
      checkboxInput_h(
        inputId = ns("show_insignificant_grey"),
        label = "Show insignificant values in grey?",
        value = FALSE
      )
    ) %nullify if% !(mode %in% c("VOLCANO")),
    checkboxInput_h(
      inputId = ns("hide_insignificant"),
      label = "Hide insignificant values?",
      value = FALSE
    ) %nullify if% !(mode %in% c("WOODS")),
    checkboxInput_h(
      inputId = ns("show_length"),
      label = "Show peptide length and position in the sequence? ",
      value = TRUE
    ) %nullify if% (mode != "MANHATTAN"),
    checkboxInput_h(
      inputId = ns("show_aggregated"),
      label = "Show aggregated data? ",
      value = TRUE
    ) %nullify if% (mode != "UNCERTAINTY"),
    checkboxInput_h(
      inputId = ns("split_timepoints"),
      label = "Show time points separately?",
      value = FALSE
    ) %nullify if% !(mode %in% c("MANHATTAN", "UNCERTAINTY")),
    tagList(
      checkboxInput_h(
        inputId = ns("show_replicates"),
        label = "Show replicate values?",
        value = FALSE
      ),
      checkboxInput_h(
        inputId = ns("log_x"),
        label = "Logaritmic x scale for Mass UPTAKE Plot?",
        value = FALSE
      )
    ) %nullify if% !(mode == "MEASUREMENTS"),
    selectizeInput(
      inputId = ns("show_residues"),
      label = "Select group of residues to display: ",
      choices = c("Hydrophilic and hydrophobic", "Only hydrophilic", "Only hydrophobic"),
      selected = "Hydrophilic and hydrophobic",
      options = list(dropdownParent = 'body')
    ) %nullify if% (mode != "SEQUENCE DATA"),
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
      c(
        if (mode == "CHICLET") list(show_uncertainty = input_r("show_uncertainty")) else NULL,
        if (mode %in% c("UPTAKE", "BUTTERFLY")) list(uncertainty_type = input_r("uncertainty_type")) else NULL,
        if (mode == "UPTAKE") list(log_x = input_r("log_x")) else NULL,
        if (mode == "VOLCANO") list(
          shown_interval = input_r("shown_interval"),
          distinguish_timepoints = input_r("distinguish_timepoints"),
          show_insignificant_grey = input_r("show_insignificant_grey")
        ) else NULL,
        if (mode %in% c("VOLCANO", "WOODS")) list(
          hide_insignificant = input_r("hide_insignificant")
        ),
        if (mode == "MANHATTAN") input_r_list("show_length", "split_timepoints"),
        if (mode == "UNCERTAINTY") input_r_list("show_aggregated", "split_timepoints"),
        if (mode == "MEASUREMENTS") input_r_list("log_x", "show_replicates"),
        if (mode == "SEQUENCE DATA") input_r_list("show_residues")
      )
    )
  })
}
