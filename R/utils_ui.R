#' Indicate that element can be toggled
#'
#' @param ... html elements
#' @param id id of the toggleable
#'
#' @examples
#' toggleable(span("you can toggle this"), id = "elem")
#'
#' \dontrun{
#' # use in server context
#' toggle_id(TRUE, "elem")
#' }
#'
#' @noRd
toggleable <- function(..., id) div(..., id = id)

add_fancy_icon <- function(fancy_icon)
  icon(fancy_icon, class = "fancy-icon")

#' Function to automatically create settings modules in ui
#'
#' @param names name of servers to install
#' @param modes named vector of modes to pass to corresponding uis
#' @param params named list of parameters to pass to corresponding uis
#' @param ns namespace function
#'
#' @noRd
install_settings_ui <- function(names, modes = character(), params = list(), ns) {
  hadex_gui_env <- rlang::ns_env("HaDeXGUI")
  uis <- purrr::map(names, function(name) {
    ui_fun <- hadex_gui_env[[glue::glue("mod_settings_{name}_ui")]]
    arg_names <- rlang::fn_fmls_names(ui_fun)

    args <- rlang::set_names(purrr::map(
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

    args <- args[purrr::map_lgl(args, not_null)]

    rlang::exec(ui_fun, !!!args)
  })
  rlang::exec(hadex_panel_settings, !!!uis)
}

#' Shorthand for column(width = 6, ...)
#'
#' @noRd
column_6 <- function(...) column(width = 6, ...)
