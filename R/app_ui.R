#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom reactlog reactlog_module_ui
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      title = "HaDeX",

      mod_page_start_ui("start_page"),
      mod_source_reading_ui("source_reading"),
      navbarMenu(
        title = "Deuterium uptake",
        mod_plot_butterfly_ui("butterfly", differential = FALSE),
        mod_plot_butterfly_ui("butterfly_diff", differential = TRUE)
      ),

      if (getOption("shiny.reactlog", default = FALSE))
        tabPanel(title = "reactlog", reactlog_module_ui())
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
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
    golem::activate_js()
  )
}
