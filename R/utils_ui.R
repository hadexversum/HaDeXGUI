#' Hide or display a tag or whole list of tags
#'
#' @param x the tag or the tag list
#'
#' @details Those functions are adapted from automatically generated utility
#' functions from golem package
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(x) {
  UseMethod("undisplay")
}

undisplay.shiny.tag <- function(x) {
  # if not already hidden
  if (
    !is.null(x$attribs$style) &&
      !grepl("display:\\s+none", x$attribs$style)
  ) {
    x$attribs$style <- paste(
      "display: none;",
      x$attribs$style
    )
  } else {
    x$attribs$style <- "display: none;"
  }
  x
}

undisplay.shiny.tag.list <- function(x) {
  force(x)
  ret <- lapply(x, undisplay)
  class(ret) <- class(x)
  ret
}

#' @importFrom shiny tagList
display <- function(x) {
  UseMethod("display")
}

display.shiny.tag <- function(x) {
  if (
    !is.null(x$attribs$style) &&
      grepl("display:\\s+none", x$attribs$style)
  ) {
    x$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      x$attribs$style
    )
  }
  x
}

display.shiny.tag.list <- function(x) {
  ret <- lapply(x, display)
  class(ret) <- class(x)
  ret
}

`%visible if%` <- function(tag, condition)
  (if (condition) display else undisplay)(tag)

.S3method("undisplay", "shiny.tag", undisplay.shiny.tag)
.S3method("undisplay", "shiny.tag.list", undisplay.shiny.tag.list)
.S3method("display", "shiny.tag", display.shiny.tag)
.S3method("display", "shiny.tag.list", display.shiny.tag.list)
