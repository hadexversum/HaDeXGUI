#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom shinyhelper helper
match_helper_content <- function(id) {
  split <- strsplit(id, split = "-|_")[[1]]
  n <- length(split)

  ret <- NULL

  # looking for helpers from most specific (prepended with module names) to least
  for (i in 1:n) {
    sub_id <- paste0(split[i:n], collapse = "-")
    if (app_sys(glue::glue("app/helpfiles/{sub_id}.md")) != "") {
      ret <- sub_id
      break
    }
  }
  ret
}

decorate_with_helper <- function(fun) function(id, ...) {
  content <- match_helper_content(id)
  if (not_null(content)) shinyhelper::helper(
    shiny_tag = fun(id, ...),
    content = content,
    type = "markdown",
    buttonLabel = "Close",
    easyClose = TRUE,
    icon = "far fa-question-circle",
    colour = "#856C9D"
  ) else fun(id, ...)
}

decorate_with_hadex_spinner <- function(fun)
  function(id, ...) shinycustomloader::withLoader(
    fun(id, ...),
    type = "image",
    loader = "www/loader.gif"
  )

# manually decorating to specify default parameter values

selectizeInput_h <- function(inputId, label, choices = "", ...,
                             options = list(dropdownParent = "body"))
  decorate_with_helper(shiny::selectizeInput)(inputId, label = label, choices = choices, options = options, ...)

textInput_h <- function(inputId, ...)
  decorate_with_helper(shiny::textInput)(inputId, ...)

checkboxInput_h <- function(inputId, ...)
  decorate_with_helper(shiny::checkboxInput)(inputId, ...)

numericInput_h <- function(inputId, label, value = 0, ...)
  decorate_with_helper(shiny::numericInput)(inputId, label, value, ...)

checkboxGroupInput_h <- function(inputId, label, choices = "", ...)
  decorate_with_helper(shiny::checkboxGroupInput)(inputId, label, choices, ...)

plotOutput_h <- function(outputId, ...)
  decorate_with_helper(decorate_with_hadex_spinner(shiny::plotOutput))(outputId, ...)

uiOutput_h <- function(outputId, ...)
  decorate_with_helper(decorate_with_hadex_spinner(shiny::uiOutput))(outputId, ...)

dataTableOutput_h <- function(outputId, ...)
  decorate_with_helper(decorate_with_hadex_spinner(DT::dataTableOutput))(outputId, ...)

girafeOutput_h <- function(outputId, ...)
  decorate_with_helper(decorate_with_hadex_spinner(ggiraph::girafeOutput))(outputId, ...)
