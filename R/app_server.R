#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom icecream ic_disable ic_enable
#' @importFrom reactlog reactlog_module_server
#' @noRd
app_server <- function(input, output, session) {
  apply_server_settings()

  dat_source <- mod_data_load_server("data_load")

  mod_plot_volcano_server(
    id = "volcano",
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_butterfly_server(
    id = "butterfly",
    differential = FALSE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_butterfly_server(
    id = "butterfly_diff",
    differential = TRUE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_chiclet_server(
    id = "chiclet",
    differential = FALSE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_chiclet_server(
    id = "chiclet_diff",
    differential = TRUE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_uptake_server(
    id = "uptake",
    differential = FALSE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  mod_plot_uptake_server(
    id = "uptake_diff",
    differential = TRUE,
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  if (getOption("shiny.reactlog", default = FALSE)) reactlog_module_server()
}
