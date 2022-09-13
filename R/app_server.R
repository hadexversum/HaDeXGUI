#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  apply_server_settings()

  # USING LOGO AS LINK TO START PAGE
  # TODO: do it with js instead
  bindEvent(observe({
    updateTabsetPanel(
      session = session,
      inputId = "navbar",
      selected = "start"
    )
  }), { input[["logo_link"]] })

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
      "uncertainty",
      ### MEASUREMENTS
      "measurements",
      ### SEQUENCE DATA
      "sequence_data",
      "coverage",
      "coverage_heatmap"
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
    dat_export = dat_export,
    dat_summary = dat_summary,
    input_info = dat_source[["input_info"]],
    params = dat_source[["params"]]
  )

  if (getOption("shiny.reactlog", default = FALSE) && is_installed("reactlog"))
    reactlog::reactlog_module_server()
}

#' Apply some global settings on server startup
#'
#' @noRd
apply_server_settings <- function() {
  if (getOption("golem.app.prod") && is_installed("icecream")) {
    icecream::ic_enable()
    options(icecream.always.include.context = TRUE)
  }
  options(hadex_use_interactive_plots = TRUE)
  ggplot2::theme_set(hadex_ggtheme())
  shinyhelper::observe_helpers(help_dir = app_sys("app/helpfiles"))
}
