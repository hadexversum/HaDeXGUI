#' Function to toggle visibility of elements
#'
#' see toggleable
#'
#' @noRd
toggle_id <- function(condition, id) {
  golem::invoke_js(
    if (condition) "showid" else "hideid",
    id
  )
}

#' Wrap DT object with unified style
#'
#' @noRd
hadex_datatable <- function(dat, cols = colnames(dat), dom = "tBip") {
  DT::datatable(
    data = dat,
    colnames = cols,
    class = "table-bordered table-condensed",
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      dom = dom,
      autoWidth = TRUE,
      buttons = c("excel", "pdf")
    ),
    filter = "bottom",
    rownames = FALSE
  )
}

#' Invoke multiple settings servers with automatic parameter binding
#'
#' @param names [character]
#'     a vector of names of servers. This should be only the direct name of a
#'     server, eg. name of `mod_settings_*_server` should be provided as
#'     `"*"`.
#' @param modes [named character]
#'     a list of modes passed to corresponding servers. Empty vector by default.
#'     by default.
#' @param env [environment]
#'     an environment in which the servers should be installed. Calling
#'     environment by default.
#'
#' @details This function is meant to be called for its side effects. Calling
#' multiple servers with parameters of standard names is equivalent to calling
#' this function. This function creates one object named `s_*` per each server
#' name provided, where `*` is the name of the server. This object is a return
#' value of the server.
#'
#' @noRd
invoke_settings_servers <- function(names, modes = character(), env = rlang::caller_env()) {
  hadex_gui_env <- rlang::ns_env("HaDeXGUI")
  purrr::walk(names, function(name){
    # transform name into server function
    server_fun <- hadex_gui_env[[glue::glue("mod_settings_{name}_server")]]
    # get names of arguments to the function
    arg_names <- rlang::fn_fmls_names(server_fun)

    # call helper function
    args <- rlang::set_names(
      # bind appropriate value to each parameter
      purrr::map(
        arg_names,
        function(arg) {
          # id is the special case, because we have the value provided directly
          if (arg == "id") name
          # if mode argument is provided in modes, use it; otherwise default to NULL
          else if (arg == "mode") {
            modes[[name]] %.?% (name %in% names(modes))
            # if name of arg starts with p_, the reactive value is present in
            # params object available in main server
          } else if (startsWith(arg, "p_")) get("params", env)[[substring(arg, 3)]]
          # otherwise, argument should be available via the calling environment
          else get(arg, envir = env)
        }
      ),
      arg_names
    )

    # remove parameters with NULL values from the call
    args <- args[purrr::map_lgl(args, not_null)]

    # call the server and do the assignment in a given environment
    rlang::env_bind(env, !!(glue::glue("s_{name}")) := rlang::exec(server_fun, !!!args))
  })
  invisible()
}

#' Function to invoke multiple plot servers in main server
#'
#' @param server_names names of the plot servers
#' @param dat_source list of reactive values returned by module_data_load
#' @param env environment to invoke servers in, defaulting to parent frame
#'
#' If server has differential parameter, it runs two servers with both possible
#' values for the variable: TRUE and FALSE and append the former server name
#' with "_diff"
#'
#' @noRd
invoke_plot_servers <- function(server_names, dat_source, env = rlang::caller_env()) {
  ret <- list()
  hadex_gui_env <- rlang::ns_env("HaDeXGUI")
  purrr::walk(server_names, function(name) {
    # transform name into server function
    server_fun <- hadex_gui_env[[glue::glue("mod_plot_{name}_server")]]
    # get names of arguments to the function
    arg_names <- rlang::fn_fmls_names(server_fun)

    args <- list()
    args[["id"]] <- name

    if ("dat" %in% arg_names) args[["dat"]] <- dat_source[["dat"]]
    if ("params" %in% arg_names) args[["params"]] <- dat_source[["params"]]

    if ("differential" %in% arg_names) {
      args[["differential"]] <- FALSE
      ret[[name]] <- rlang::exec(server_fun, !!!args)

      args[["differential"]] <- TRUE
      name <- glue::glue("{name}_diff")
      args[["id"]] <- name
      ret[[name]] <- rlang::exec(server_fun, !!!args)

    } else {
      ret[[name]] <- rlang::exec(server_fun, !!!args)
    }
  })
  ret
}

#' Function to return plot and data from plot server modules
#'
#' @param ... multiple strings containing specific names of plot
#'
#' @return If ... is empty, function returns list of two elements named
#' "plot" and "dat", containing plot and data table generated within calling
#' module. If ... is non-empty, "plot" and "dat" are list of plots and list of
#' tables with names corresponding to names passed in ...
#'
#' @noRd
autoreturn <- function(..., env = rlang::caller_env()) {
  plot_names <- list(...)
  if (length(plot_names) == 0) {
    list(
      plot = get("plot_out", envir = env),
      dat = get("dat_out", envir = env)
    )
  } else {
    plot_names <- as.character(plot_names)
    list(
      plot = rlang::set_names(purrr::map(plot_names, ~ get(glue::glue("plot_out_{.x}"), envir = env)), plot_names),
      dat = rlang::set_names(purrr::map(plot_names, ~ get(glue::glue("dat_out_{.x}"), envir = env)), plot_names)
    )
  }
}
