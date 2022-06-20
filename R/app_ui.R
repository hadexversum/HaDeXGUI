#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      title = "HaDeX",
      tabPanel(
        title = "Start",
        h1("Welcome to HaDeX GUI!"),
        fluidPage(
          fluidRow(
            column(
              width = 5,
              h6("Thank you for using our tool."),
              h6("Questions/feature requests/commercial applications: hadex@ibb.waw.pl")
            ),
            column(
              width = 5,
              span("This is the development version of the application and may not be working correctly yet. The official version will be available soon. ", style="color:red")
            )
          )
        ),
        includeMarkdown(app_sys("app/man/about.md")),
        img(
          id = "HaDeX-funding-icons",
          src = "www/funding_icons.png"
        )
      ),
      mod_source_reading_ui("source_reading")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HaDeXGUI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
