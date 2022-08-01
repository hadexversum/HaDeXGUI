#' Finds suitable name of helper for given input/output id
#'
#' @param id id of the input/output
#'
#' Function splits id by "-" and "_" symbols and tries to match file name in
#' app/helpfiles folder with the longest name possible. E.g. if you have id
#' "some_server-some_input" it will look for following files in order:
#'
#' - some-server-some-input.md
#' - server-some-input.md
#' - some-input.md
#' - input.md
#'
#' In this way, you can create a helpfiles meant precisely for a particular input
#' in particular tab and have a fallback with general description.
#'
#' @return name of the file containing helper for input (without full path and
#' extension)
#'
#' @noRd
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

#' Wrap output with hadex spinner
#'
#' @param tag a shiny tag
#'
#' @examples
#' hadex_with_spinner(plotOutput("some_id"))
#'
#' @noRd
hadex_with_spinner <- function(tag) shinycustomloader::withLoader(
  tag,
  type = "image",
  loader = "www/loader.gif"
)

#' Wrap output with hadex helper
#'
#' @param tag a shiny tag
#' @param content name of the helpfile; if NULL no helper is created
#'
#' @examples
#' hadex_wiht_helper(plotOutput("some_id"), "a-file")
#'
#' @noRd
hadex_with_helper <- function(tag, content) if (not_null(content)) shinyhelper::helper(
  shiny_tag = tag,
  content = content,
  type = "markdown",
  buttonLabel = "Close",
  easyClose = TRUE,
  icon = "far fa-question-circle",
  colour = "#856C9D"
) else tag

#' Function for decorating inputs/ouptuts
#'
#' @param fun input/output function to decorate
#' @param with_helper should helper be added to the tag?
#' @param is_output is the wrapped function input/output?
#' @param with_spinner should the spinner be added to function?
#'
#' @return a function for creating inputs/outputs
#'
#' This is a function meant to use only on development level, it creates inputs
#' and outputs from other inputs/outputs. It operates on the function level,
#' not on the particular tag level.
#' See examples in file utils_helpers.R
#'
#' @noRd
hadex_decorate <- function(fun, with_helper = TRUE, is_output = TRUE, with_spinner = is_output) {
  spinner_wrapper <- if (with_spinner) hadex_with_spinner else identity
  helper_wrapper <- if (with_helper) function(tag, id) {
    content <- match_helper_content(id)
    hadex_with_helper(tag, content)
  } else function(tag, id) tag

  # TODO: make use of rlang::fmls
  if (is_output) rlang::inject({
    function(outputId, ...) {
      (!!(helper_wrapper))((!!(spinner_wrapper))(fun(outputId, ...)), outputId)
    }
  }) else rlang::inject({
    function(inputId, ...) {
      (!!(helper_wrapper))((!!(spinner_wrapper))(fun(inputId, ...)), inputId)
    }
  })
}


# manually decorating to specify default parameter values

selectizeInput_h <- function(inputId, label, choices = "", ...,
                             options = list(dropdownParent = "body"))
  hadex_decorate(shiny::selectizeInput, is_output = FALSE)(inputId, label = label, choices = choices, options = options, ...)

textInput_h <- hadex_decorate(shiny::textInput, is_output = FALSE)
checkboxInput_h <- hadex_decorate(shiny::checkboxInput, is_output = FALSE)

numericInput_h <- function(inputId, label, value = 0, ...)
  hadex_decorate(shiny::numericInput, is_output = FALSE)(inputId, label, value, ...)

checkboxGroupInput_h <- function(inputId, label, choices = "", ...)
  hadex_decorate(shiny::checkboxGroupInput, is_output = FALSE)(inputId, label, choices, ...)

plotOutput_h <- hadex_decorate(shiny::plotOutput)
uiOutput_h <- hadex_decorate(shiny::uiOutput)
dataTableOutput_h <- hadex_decorate(DT::dataTableOutput)
girafeOutput_h <- hadex_decorate(ggiraph::girafeOutput)
