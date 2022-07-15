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

toggleable <- function(..., id) div(..., id = id)

`%nullify if%` <- function(x, condition) if (condition) NULL else x

.S3method("undisplay", "shiny.tag", undisplay.shiny.tag)
.S3method("undisplay", "shiny.tag.list", undisplay.shiny.tag.list)
.S3method("display", "shiny.tag", display.shiny.tag)
.S3method("display", "shiny.tag.list", display.shiny.tag.list)

gen_random_id <- function(prefix = "")
  paste0(prefix, paste0(sample(c(0:9, letters[1:6]), 16, TRUE), collapse = ''))

add_fancy_icon <- function(fancy_icon)
  icon(fancy_icon, class = "fancy-icon")

construct_var_name <- function(diff, theo, frac, var)
  paste0(
    "diff_" %nullify if% (!diff),
    "theo_" %nullify if% (!theo),
    "frac_" %nullify if% (!frac),
    var
  )

install_settings_ui <- function(names, modes, params = list(), ns) {
  uis <- lapply(names, function(name) {
    ui_fun <- getFromNamespace(paste0("mod_settings_", name, "_ui"), "HaDeXGUI")
    arg_names <- names(formals(ui_fun))

    args <- setNames(lapply(
      arg_names,
      function(arg) {
        if (arg == "id") ns(name)
        else if (arg == "mode") {
          if (name %in% names(modes)) modes[[name]] else NULL
        } else {
          if (arg %in% names(params)) params[[arg]] else NULL
        }
      }
    ), arg_names)

    args <- args[sapply(args, not_null)]

    do.call(ui_fun, args = args)
  })
  rlang::exec(hadex_panel_settings, !!!uis)
}
