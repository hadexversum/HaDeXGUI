#' page_about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_about_ui <- function(id){
  hadex_tab_other(
    title = "About",

    fluidPage(
      fluidRow(
        column(
          width = 5,
          includeMarkdown(app_sys("app/man/about.md"))
        ),
        column(
          width = 6,
          includeMarkdown(app_sys("app/man/resources.md"))
        )
      )
    )
  )
}
