#' Returns first, last, middle element of a generalised vector
#'
#' @param x a generalised vector
#'
#' If length of x is even, middle returns element of next index higher than half
#'
#' @examples
#' first(1:5)
#' last(list(1, 2, 3))
#' middle(LETTERS)
#'
#' @noRd
first <- function(x) x[[1]]

last <- function(x) x[[length(x)]]

middle <- function(x) x[[ceiling(length(x) / 2)]]

#' Inverted version of is.null
#'
#' Automatically created with golem
#'
#' @noRd
#'
#' @examples
#' not_null(NULL)
not_null <- Negate(is.null)

#' If x is `NULL`, return `NULL`, otherwise return f(x)
#'
#' @param x [any] \n Element to test, potentially `NULL`
#' @param f [function] \n A function to be applied to x.
#'
#' @noRd
#'
#' @examples
#' 5 %?>% exp
#' NULL %?>% exp
`%?>%` <- function(x, f) if (is.null(x)) NULL else f(x)

`%nullify if%` <- function(x, condition) if (condition) NULL else x

gen_random_id <- function(prefix = "")
  paste0(prefix, paste0(sample(c(0:9, letters[1:6]), 16, TRUE), collapse = ''))
