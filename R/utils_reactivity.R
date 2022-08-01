#' Get input as a reactive value
#'
#' @param name [character(1)]
#'     Name of the input value.
#' @param env the environment
#' @param ... Any number of characters or character vectors
#'
#' `input_r` is a helper function that automatically wraps input as a reactive
#' value. `input_r_numeric` does the same, but additionally applies `as.numeric`
#' transformation on input. `input_r_list` takes an arbitrary number of
#' characters and returns a named list of reactive values, which is equivalent
#' to manually applying `input_r` to each of the strings.
#'
#' @noRd
input_r <- function(name, env = rlang::caller_env())
  rlang::inject(reactive({ input[[!!name]] }, env = env))

input_r_numeric <- function(name, env = rlang::caller_env())
  rlang::inject(reactive({ as.numeric(input[[!!name]]) }, env = env))

input_r_list <- function(..., env = rlang::caller_env()) {
  names <- unlist(list(...))
  rlang::set_names(purrr::map(names, input_r, env = env), names)
}

#' Function to wait for parameters
#'
#' @noRd
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
#'
#' @noRd
`%()%` <- function(lhs, rhs) lhs[[rlang::expr_deparse(rlang::enexpr(rhs))]]()
