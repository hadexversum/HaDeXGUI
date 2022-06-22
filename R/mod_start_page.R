#' start_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_page_ui <- function(id){
  HaDeX_nonplotTab(
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
  )
}
