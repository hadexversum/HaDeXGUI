#' Get input as a reactive value
#'
#' @param name [character(1)]
#'     Name of the input value.
#' @param env the environment
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

#' @rdname input_r
input_r_numeric <- function(name, env = parent.frame()) {
  env <- new.env(parent = env)
  eval(rlang::expr(reactive({ as.numeric(input[[!!name]]) }, env = env)))
}

#' @param ... Any number of characters or character vectors
#' @rdname input_r
input_r_list <- function(..., env = parent.frame()) {
  names <- unlist(list(...))
  setNames(lapply(names, input_r, env = env), names)
}

wait_for <- function(expr, message = "Wait for the parameters to be loaded...") {
  validate(need(expr, message))
}

#' Access value of list of reactives
#'
#' Because typing `some_list[["some_values"]]()` takes too long
#'
#' @param lhs [list]
#'     list of reactive values
#' @param rhs [symbol]
#'     name of the element of the list being the reactive value, unquoted
#' @return A value of reactive expression
`%()%` <- function(lhs, rhs) lhs[[deparse(substitute(rhs))]]()
