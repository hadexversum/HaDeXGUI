#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom reactlog reactlog_module_ui
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      id = "navbar",
      title = "HaDeX",

      mod_page_start_ui("page_start"),
      mod_data_load_ui("data_load"),
      navbarMenu(
        title = "Deuterium uptake",
        mod_plot_comparison_and_woods_ui("comparison_and_woods"),
        mod_plot_volcano_ui("volcano"),
        navbarMenu(
          title = "Butterfly",
          mod_plot_butterfly_ui("butterfly", differential = FALSE),
          mod_plot_butterfly_ui("butterfly_diff", differential = TRUE),
        ),
        navbarMenu(
          title = "Chiclet",
          mod_plot_chiclet_ui("chiclet", differential = FALSE),
          mod_plot_chiclet_ui("chiclet_diff", differential = TRUE),
        ),
        mod_plot_uptake_ui("uptake", differential = FALSE),
        mod_plot_uptake_ui("uptake_diff", differential = TRUE)
      ),
      navbarMenu(
        title = "Time-based data",
        mod_plot_replicates_ui("replicates"),
        mod_plot_manhattan_ui("manhattan"),
        mod_plot_uncertainty_ui("uncertainty")
      ),
      mod_plot_measurements_ui("measurements"),
      navbarMenu(
        title = "Sequence data",
        mod_plot_sequence_data_ui("sequence_data"),
        mod_plot_coverage_ui("coverage"),
        mod_plot_coverage_heatmap_ui("coverage_heatmap")
      ),
      mod_page_summary_ui("page_summary"), #TODO: shouldn't this tab be merged with Sequence data?
      mod_report_ui("report"),
      mod_page_about_ui("page_about"),

      if (getOption("shiny.reactlog", default = FALSE))
        tabPanel(title = "reactlog", reactlog_module_ui()),

      # TODO: maybe make some more clever injection?
      header = actionLink(
        "logo_link",
        "",
        NULL,
        img(id = "HaDeX-logo", src = "www/logo.png")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "utils",
    app_sys("app/utils")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HaDeXGUI"
    ),

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    golem::activate_js(),
    tags$script(src = "utils/nested-dropdowns.js", type = "text/javascript")
  )
}
