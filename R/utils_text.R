capitalize <- function(x) {
  first_letter <- substr(x, 1, 1)
  substr(x, 1, 1) <- toupper(first_letter)
  x
}

react_construct_uptake_lab_y <- function(differential, env = parent.frame()) rlang::inject(
  reactive({
    frac_str <- if (s_calculation[["fractional"]]()) "Fractional " else ""
    diff_str <- if (!!differential) "difference [%]" else "[Da]"
    capitalize(glue("{frac_str}deuterium uptake {diff_str}"))
  }, env = env)
)

#' @importFrom glue glue
react_construct_uptake_title <- function(plot_type, differential, include_state = TRUE,
                                         env = parent.frame()) rlang::inject(
  reactive({
    plot_type <- !!plot_type
    theo_str <- if (s_calculation[["theoretical"]]()) "Theoretical " else ""
    states_str <- if (!!differential) {
      glue("between {s_state[['state_1']]()} and {s_state[['state_2']]()}")
    } else {
      state_str <- if (!!include_state) glue("for state {s_state[['state']]()} ") else ""
      glue("{state_str}for {params[['chosen_protein']]()}")
    }

    capitalize(glue("{theo_str}{plot_type} plot {states_str}"))

  }, env = env)
)

construct_auto_range_lab <- function(plot_label, ax, differential = FALSE)
  glue::glue("Choose {ax} range for the {plot_label} {if (differential) 'Differential ' else ''}Plot:")

construct_auto_range_labs <- function(plot_label, axes = c("x", "y"), differential = FALSE)
  sapply(axes, function(ax) construct_auto_range_lab(plot_label, ax, differential), USE.NAMES = TRUE)
