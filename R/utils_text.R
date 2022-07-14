capitalize <- function(x) {
  first_letter <- substr(x, 1, 1)
  substr(x, 1, 1) <- toupper(first_letter)
  x
}

decapitalize <- function(x) {
  first_letter <- substr(x, 1, 1)
  substr(x, 1, 1) <- tolower(first_letter)
  x
}

react_construct_uptake_lab_y <- function(differential, env = parent.frame()) rlang::inject(
  reactive({
    frac_str <- if (s_calculation[["fractional"]]()) "Fractional " else ""
    diff_str <- if (!!differential) "difference [%]" else if (s_calculation[["fractional"]]()) "[%]" else "[Da]"
    capitalize(glue("{frac_str}deuterium uptake {diff_str}"))
  }, env = env)
)

#' @importFrom glue glue
react_construct_uptake_title <- function(plot_name, differential = FALSE, include_state = TRUE,
                                         include_exposure = FALSE,
                                         env = parent.frame()) rlang::inject(
  reactive({
    plot_name <- !!plot_name
    theo_str <- if (s_calculation[["theoretical"]]()) "Theoretical " else ""
    states_str <- if (!!differential) {
      glue("between {s_state[['state_1']]()} and {s_state[['state_2']]()}")
    } else {
      state_str <- if (!!include_state) glue("for state {s_state[['state']]()} ") else ""
      glue("{state_str}for {params[['chosen_protein']]()}")
    }
    exposure_str <- if (!!include_exposure) glue("in {s_time[['t']]()} min ") else ""

    capitalize(glue("{theo_str}{plot_name} plot {exposure_str}{states_str}"))

  }, env = env)
)


construct_plot_label <- function(plot_name, differential = FALSE)
  glue::glue("{capitalize(plot_name)} {if (differential) 'Differential ' else ''}Plot")

construct_auto_range_lab <- function(plot_name, ax, differential = FALSE)
  glue::glue("Choose {ax} range for the {construct_plot_label(plot_name, differential)}:")

construct_auto_range_labs <- function(plot_name, axes = c("x", "y"), differential = FALSE)
  sapply(axes, function(ax) construct_auto_range_lab(plot_name, ax, differential), USE.NAMES = TRUE)

construct_auto_label_lab <- function(plot_label, label_type)
  glue::glue("{plot_label} {label_type}:")

construct_auto_label_lab_set <- function(plot_label, id_prefix = NULL) {
  setNames(
    sapply(
      c("title", "axis x label", "axis y label"),
      function(label_type) construct_auto_label_lab(plot_label, label_type)
    ),
    paste0((paste0(id_prefix, "_") %nullify if% is.null(id_prefix)), c("title", "x", "y"))
  )
}

construct_auto_label_labs <- function(plot_names, differential = FALSE) {
  if (length(plot_names) == 1)
    construct_auto_label_lab_set(construct_plot_label(plot_names, differential = differential))
  else rlang::exec(
    c,
    !!!lapply(plot_names, function(plot_name) {
      construct_auto_label_lab_set(
        construct_plot_label(plot_name, differential),
        id_prefix = decapitalize(plot_name)
      )
    })
  )
}
