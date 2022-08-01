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

#' Nullify conditionally
#'
#' @param x an expression
#' @param condition condition to test
#'
#' For %.?%: if condition is true, returns x, else NULL.
#' For %.?!%: if condtiton is true, returns NULL, else x
#'
#' Note that those operators binds weekly to other expressions, so if condition
#' is compound, parentheses are required. That is the justification to having
#' two versions of the operator -- to remove some redundancy.
#'
#' @examples
#' exp(12) %.?% (5 > 0)
#' exp(12) %.?% (5 > 10)
#' exp(12) %.?!% (5 > 0)
#' exp(12) %.?!% (5 > 10)
#'
#' @noRd
`%.?%` <- function(x, condition) if (condition) x else NULL

`%.?!%` <- function(x, condition) if (condition) NULL else x

#' Get random id
#'
#' @param prefix to the random id value
#'
#' @examples
#' get_random_id()
#' get_random_id("xyz")
#'
#' @noRd
gen_random_id <- function(prefix = "")
  paste0(prefix, paste0(sample(c(0:9, letters[1:6]), 16, TRUE), collapse = ''))

#' Function that check for presence of other suggested packages
#'
#' @noRd
is_installed <- function(package)
  length(find.package(package, quiet = TRUE)) > 0
