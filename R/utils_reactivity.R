#' Get input as a reactive value
#'
#' @param name [character(1)] \n Name of the input value.
#' @param env the environment
#' @param ... Any number of characters or character vectors
#'
#' `input_r` is a helper function that automatically wraps input as a reactive
#' value. `input_r_numeric` does the same, but additionally applies `as.numeric`
#' transformation on input. `input_r_list` takes an arbitrary number of
#' characters and returns a named list of reactive values, which is equivalent
#' to manually applying `input_r` to each of the strings.
#'
#' @importFrom shiny getDefaultReactiveDomain reactive
#' @importFrom rlang expr
input_r <- function(name, env = parent.frame()) {
  env <- new.env(parent = env)
  eval(rlang::expr(reactive({ input[[!!name]] }, env = env)))
}

#' @noRd
input_r_numeric <- function(name, env = parent.frame()) {
  env <- new.env(parent = env)
  eval(rlang::expr(reactive({ as.numeric(input[[!!name]]) }, env = env)))
}

#' @noRd
input_r_list <- function(...) {
  names <- unlist(list(...))
  setNames(lapply(names, input_r, env = parent.frame()), names)
}
