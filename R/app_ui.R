#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    div(
      id = "hadex-full",
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
          mod_plot_butterfly_ui("butterfly", differential = FALSE),
          mod_plot_butterfly_ui("butterfly_diff", differential = TRUE),
          mod_plot_chiclet_ui("chiclet", differential = FALSE),
          mod_plot_chiclet_ui("chiclet_diff", differential = TRUE),
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

        if (getOption("shiny.reactlog", default = FALSE) && is_installed("reactlog"))
          tabPanel(title = "reactlog", reactlog::reactlog_module_ui()),

        # TODO: maybe make some more clever injection?
        header = actionLink(
          "logo_link",
          "",
          NULL,
          img(id = "HaDeX-logo", src = "www/logo.png")
        )
      )
    ),
    div(
      id = "hadex-mobile",
      navbarPage(
        id = "navbar",
        title = "HaDeX",
        mod_page_start_ui("page_start", mobile = TRUE),
        # TODO: maybe make some more clever injection?
        header = actionLink(
          "logo_link",
          "",
          NULL,
          img(id = "HaDeX-logo", src = "www/logo.png")
        )
      )
    ),

    # included here as a walk-around for late inclusion of DT css
    includeCSS(path = app_sys("app/utils/datatable.css")),
    includeCSS(path = app_sys("app/utils/selectize.css"))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
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
    golem::activate_js()
  )
}
