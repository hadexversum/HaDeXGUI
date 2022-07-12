#' Get input as a reactive value
#'
#' @param name [character(1)] \n Name of the input value.
#' @param env the environment
#' @param ... Any number of characters or character vectors
#'
#' `input_r` is a helper function that automatically wraps input as a reactive
#' value. `input_r_numeric` does the same, but additionally applies `as.numeric`
#' transformation on input. `input_r_list` takes an arbitrary number of
#' characters and returns a named list of reactive values, which is equivalent
#' to manually applying `input_r` to each of the strings.
#'
#' @importFrom shiny getDefaultReactiveDomain reactive
#' @importFrom rlang expr
input_r <- function(name, env = parent.frame()) {
  env <- new.env(parent = env)
  eval(rlang::expr(reactive({ input[[!!name]] }, env = env)))
}

#' @noRd
input_r_numeric <- function(name, env = parent.frame()) {
  env <- new.env(parent = env)
  eval(rlang::expr(reactive({ as.numeric(input[[!!name]]) }, env = env)))
}

#' @noRd
input_r_list <- function(...) {
  names <- unlist(list(...))
  setNames(lapply(names, input_r, env = parent.frame()), names)
}

#' Invoke multiple settings servers with automatic parameter binding
#'
#' @param names [character]
#'     a vector of names of servers. This should be only the direct name of a
#'     server, eg. name of `mod_settings_*_server` should be provided as
#'     `"*"`.
#' @param const_params [named list]
#'     a list of constant parameters passed to a servers if they are able to
#'     accept non-reactive replacements of some of their parameters. Emply list
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
invoke_settings_servers <- function(names, const_params = list(), env = parent.frame()) {
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
          # if some argument is provided as const_param, use it
          else if (arg %in% names(const_params)) const_params[[arg]]
          # if name of arg starts with p_, the reactive value is present in
          # params object available in main server
          else if (startsWith(arg, "p_")) get("params", env)[[substring(arg, 3)]]
          # otherwise, argument should be available via the calling environment
          else get(arg, envir = env)
        }
      ),
      arg_names
    )

    # call the server and do the assignment in a given environment
    assign(paste0("s_", name), do.call(server_fun, args = args), envir = env)
  }
  invisible()
}

wait_for <- function(expr, message = "Wait for the parameters to be loaded...") {
  validate(need(expr, message))
}

`%()%` <- function(lhs, rhs) lhs[[deparse(substitute(rhs))]]()
