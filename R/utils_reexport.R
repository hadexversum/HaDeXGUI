#' Function to round x to nearest multiple of given number
#'
#' @param x [numeric] a vector of numbers
#' @param accuracy [numeric] a number to round to (or a vector of numbers)
#' @param f [function] rounding function
#'
#' @details Function reexported from package `{plyr}`
#'
#' @return Value of x rounded to nearest multiple of accuracy using function f
round_any <- function(x, accuracy, f = round) f(x / accuracy) * accuracy
