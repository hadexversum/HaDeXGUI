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

  ### AUTOMATIC INVOCATION OF PLOTTING SERVERS

  dat_export <- invoke_plot_servers(
    server_names = c(
      ### DEUTERIUM UPTAKE:
      "comparison_and_woods",
      "volcano",
      "butterfly", # +diff
      "chiclet",   # +diff
      "uptake",    # +diff
      ### TIME-BASED DATA:
      "replicates",
      "manhattan",
      "quality_control",
      "uncertainty",
      ### MEASUREMENTS
      "measurements",
      ### SEQUENCE DATA
      "sequence_data",
      "coverage"
    ),
    dat_source = dat_source
  )

  ### SUMMARY

  dat_summary <- mod_page_summary_server(
    id = "page_summary",
    dat = dat_source[["dat"]],
    params = dat_source[["params"]]
  )

  ### REPORT

  mod_report_server(
    id = "report",
    dat_export = dat_export
  )

  if (getOption("shiny.reactlog", default = FALSE)) reactlog_module_server()
}
