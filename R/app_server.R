#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom icecream ic_disable ic_enable
#' @importFrom reactlog reactlog_module_server
#' @noRd
app_server <- function(input, output, session) {
  apply_server_settings()

  dat_source <- mod_source_reading_server("source_reading")

  mod_uptake_butterfly_server(
    id = "butterfly",
    differential = FALSE,
    dat = dat_source[["dat"]],
    chosen_protein = dat_source[["chosen_protein"]],
    states_chosen_protein = dat_source[["states_chosen_protein"]],
    times = dat_source[["times"]],
    times_with_control = dat_source[["times_with_control"]],
    deut_part = dat_source[["deut_part"]],
    no_deut_control = dat_source[["no_deut_control"]]
  )

  mod_uptake_butterfly_server(
    id = "butterfly_diff",
    differential = TRUE,
    dat = dat_source[["dat"]],
    chosen_protein = dat_source[["chosen_protein"]],
    states_chosen_protein = dat_source[["states_chosen_protein"]],
    times = dat_source[["times"]],
    times_with_control = dat_source[["times_with_control"]],
    deut_part = dat_source[["deut_part"]],
    no_deut_control = dat_source[["no_deut_control"]]
  )

  if (getOption("shiny.reactlog", default = FALSE)) reactlog_module_server()
}
