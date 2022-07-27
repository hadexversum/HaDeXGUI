#' @importFrom ggplot2 theme_bw theme element_rect
hadex_ggtheme <- function() {
  thm <- theme_bw()
  thm_sub <- theme(plot.background = element_rect(fill = NA, color = NA))
  thm[names(thm_sub)] <- thm_sub
  thm
}


#' @importFrom icecream ic_enable
#' @importFrom ggplot2 theme_set
apply_server_settings <- function() {
  if (getOption("golem.app.prod")) {
    ic_enable()
    options(icecream.always.include.context = TRUE)
  }

  theme_set(hadex_ggtheme())

  shinyhelper::observe_helpers(help_dir = app_sys("app/helpfiles"))
}

extract_limits_from_range <- function(range) c(range()[[1]], range()[[2]])

update_axes_and_labels <- function(
    plt, range_x = NULL, range_y = NULL,
    labels, label_prefix = NULL) {
  wrap <- if (is.null(label_prefix)) identity else function(id) paste0(label_prefix, "_", id)

  plt +
    coord_cartesian(
      xlim = range_x %?>% extract_limits_from_range,
      ylim = range_y %?>% extract_limits_from_range
    ) +
    labs(
      title = labels[[wrap("title")]](),
      x = labels[[wrap("x")]](),
      y = labels[[wrap("y")]]()
    ) +
    theme(
      plot.title = element_text(size = labels[[wrap("title_size")]]()),
      axis.text.x = element_text(size = labels[[wrap("x_size")]]()),
      axis.title.x = element_text(size = labels[[wrap("x_size")]]()),
      axis.title.y = element_text(size = labels[[wrap("y_size")]]()),
      axis.text.y = element_text(size = labels[[wrap("y_size")]]()),
      legend.text = element_text(size = labels[[wrap("x_size")]]()),
      legend.title = element_text(size = labels[[wrap("x_size")]]())
    )
}

toggle_id <- function(condition, id) {
  golem::invoke_js(
    if (condition) "showid" else "hideid",
    id
  )
}

HaDeX_DT_format <- function(dat, cols = colnames(dat), dom = "tBip") {
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
invoke_settings_servers <- function(names, modes = character(), env = parent.frame()) {
  for (name in names) {
    # transform name into server function
    server_fun <- getFromNamespace(paste0("mod_settings_", name, "_server"), "HaDeXGUI")
    # get names of arguments to the function
    arg_names <- names(formals(server_fun))

    # call helper function
    args <- setNames(
      # bind appropriate value to each parameter
      lapply(
        arg_names,
        function(arg) {
          # id is the special case, because we have the value provided directly
          if (arg == "id") name
          # if mode argument is provided in modes, use it; otherwise default to NULL
          else if (arg == "mode") {
            if (name %in% names(modes)) modes[[name]]
            else NULL
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
    args <- args[sapply(args, not_null)]

    # call the server and do the assignment in a given environment
    assign(paste0("s_", name), do.call(server_fun, args = args), envir = env)
  }
  invisible()
}

invoke_plot_servers <- function(server_names, dat_source, env = parent.frame()) {
  ret <- list()
  for (name in server_names) {
    # transform name into server function
    server_fun <- getFromNamespace(paste0("mod_plot_", name, "_server"), "HaDeXGUI")
    # get names of arguments to the function
    arg_names <- names(formals(server_fun))

    args <- list()
    args[["id"]] <- name

    if ("dat" %in% arg_names) args[["dat"]] <- dat_source[["dat"]]
    if ("params" %in% arg_names) args[["params"]] <- dat_source[["params"]]

    if ("differential" %in% arg_names) {
      args[["differential"]] <- FALSE
      ret[[name]] <- rlang::exec(server_fun, !!!args)

      args[["differential"]] <- TRUE
      name <- paste0(name, "_diff")
      args[["id"]] <- name
      ret[[name]] <- rlang::exec(server_fun, !!!args)

    } else {
      ret[[name]] <- rlang::exec(server_fun, !!!args)
    }
  }
  ret
}

autoreturn <- function(..., env = parent.frame()) {
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
