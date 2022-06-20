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
  mod_source_reading_server("source_reading")
}
