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
    states_chosen_protein = dat_source[["states_chosen_protein"]],
    times_from_file = dat_source[["times_from_file"]],
    times_with_control = dat_source[["times_with_control"]]
  )
}
