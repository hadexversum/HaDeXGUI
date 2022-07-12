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

wrap_id <- function(id, type) paste0(type, "-", id)
wrap_div <- function(..., id, type) div(..., id = wrap_id(id, type))

`%nullify if%` <- function(x, condition) if (condition) NULL else x

.S3method("undisplay", "shiny.tag", undisplay.shiny.tag)
.S3method("undisplay", "shiny.tag.list", undisplay.shiny.tag.list)
.S3method("display", "shiny.tag", display.shiny.tag)
.S3method("display", "shiny.tag.list", display.shiny.tag.list)

gen_random_id <- function(prefix = "")
  paste0(prefix, paste0(sample(c(0:9, letters[1:6]), 16, TRUE), collapse = ''))

add_fancy_icon <- function(fancy_icon)
  icon(fancy_icon, class = "fancy-icon")

collapsible_card <- function(title, ..., init_collapsed = FALSE, id = NULL, fancy_icon = NULL) {
  obj_id = if (!is.null(id)) id else gen_random_id("collapsible_")

  tagList(
    singleton(tags$head(tags$script(src = "utils/collapse.js", type = "text/javascript"))),
    singleton(tags$head(tags$link(href = "utils/collapse.css", rel = "stylesheet"))),

    div(
      h6(
        fancy_icon %?>% add_fancy_icon,
        p(title),
        icon(if (init_collapsed) "angle-down" else "angle-up", class = "arrow-icon"),
        class = if (init_collapsed) "card-header collapsed" else "card-header",
        `data-toggle` = "collapse",
        `data-target` = paste0("#", obj_id),
        `aria-expanded` = if (init_collapsed) "false" else "true"
      ),
      div(
        ...,
        id = obj_id,
        class = if (init_collapsed) "card-body collapse" else "card-body collapse in",
        `aria-expanded` = if (init_collapsed) "false" else "true"
      ),
      class = "collapsible-card"
    )
  )
}

construct_var_name <- function(diff, theo, frac, var)
  paste0(
    "diff_" %nullify if% (!diff),
    "theo_" %nullify if% (!theo),
    "frac_" %nullify if% (!frac),
    var
  )

install_settings_ui <- function(names, modes, params, ns) {
  lapply(names, function(name) {
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
}
