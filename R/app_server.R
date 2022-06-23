#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom icecream ic_disable ic_enable
#' @noRd
app_server <- function(input, output, session) {
  if (getOption("golem.app.prod"))
    ic_disable()
  else {
    options(icecream.always.include.context = TRUE)
    ic_enable()
  }
  dat_source <- mod_source_reading_server("source_reading")

  mod_uptake_butterfly_server("uptake_butterfly",
    dat = dat_source[["dat"]],
    chosen_protein = dat_source[["chosen_protein"]],
    states_chosen_protein = dat_source[["states_chosen_protein"]],
    times = dat_source[["times"]],
    times_with_control = dat_source[["times_with_control"]],
    deut_part = dat_source[["deut_part"]],
    no_deut_control = dat_source[["no_deut_control"]]
  )
}
