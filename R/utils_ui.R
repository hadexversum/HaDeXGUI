toggleable <- function(..., id) div(..., id = id)

`%nullify if%` <- function(x, condition) if (condition) NULL else x

gen_random_id <- function(prefix = "")
  paste0(prefix, paste0(sample(c(0:9, letters[1:6]), 16, TRUE), collapse = ''))

add_fancy_icon <- function(fancy_icon)
  icon(fancy_icon, class = "fancy-icon")

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

column_6 <- function(...) column(width = 6, ...)
