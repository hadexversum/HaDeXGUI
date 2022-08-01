#' Sets the first letter of a word as capital or non-capital
#'
#' @examples
#' capitalize("asdf")
#' decapitalize("Asdf")
#'
#' @noRd
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

#' Make id-compliant string from name-like value
#'
#' @examples
#' idize("some text")
#'
#' @noRd
idize <- function(x) gsub(" ", "_", tolower(x))

#' Create reactive value with y label for generalised uptake plots
#'
#' Requires s_calculation reactive lists in calling environment
#'
#' @noRd
react_construct_uptake_lab_y <- function(differential, env = parent.frame()) rlang::inject(
  reactive({
    frac_str <- if (s_calculation[["fractional"]]()) "Fractional " else ""
    diff_str <- if (!!differential) "difference [%]" else if (s_calculation[["fractional"]]()) "[%]" else "[Da]"
    capitalize(glue::glue("{frac_str}deuterium uptake {diff_str}"))
  }, env = env)
)

#' Create reactive value generating uptake plot titles
#'
#' Requires s_time, s_state, params and s_calculation reactive lists
#' conditionally, basing on inclusion parameters
#' @noRd
react_construct_uptake_title <- function(plot_name, differential = FALSE, include_state = TRUE,
                                         include_exposure = FALSE,
                                         env = parent.frame()) rlang::inject(
  reactive({
    plot_name <- !!plot_name
    theo_str <- if (s_calculation[["theoretical"]]()) "Theoretical " else ""
    states_str <- if (!!differential) {
      glue::glue("between {s_state[['state_1']]()} and {s_state[['state_2']]()}")
    } else {
      state_str <- if (!!include_state) glue::glue("for state {s_state[['state']]()} ") else ""
      glue::glue("{state_str}for {params[['chosen_protein']]()}")
    }
    exposure_str <- if (!!include_exposure && !s_time[["multiple"]]()) glue::glue("in {s_time[['t']]()} min ") else ""

    capitalize(glue::glue("{theo_str}{plot_name} plot {exposure_str}{states_str}"))

  }, env = env)
)

#' Automatially construct labels for plots, ranges and labels (sic!)
#'
#' @noRd
construct_plot_label <- function(plot_name, differential = FALSE)
  glue::glue("{capitalize(plot_name)} {if (differential) 'Differential ' else ''}Plot")

construct_auto_range_lab <- function(plot_name, ax, differential = FALSE)
  glue::glue("Choose {ax} range for the {construct_plot_label(plot_name, differential)}:")

construct_auto_range_labs <- function(plot_name, axes = c("x", "y"), differential = FALSE)
  rlang::set_names(purrr::map_chr(axes, ~ construct_auto_range_lab(plot_name, .x, differential)), axes)

construct_auto_label_lab <- function(plot_label, label_type)
  glue::glue("{plot_label} {label_type}:")

construct_auto_label_lab_set <- function(plot_label, id_prefix = NULL) {
  rlang::set_names(
    purrr::map_chr(
      c("title", "axis x label", "axis y label"),
      ~ construct_auto_label_lab(plot_label, .x)
    ),
    paste0((glue::glue("{id_prefix}_") %.?% not_null(id_prefix)), c("title", "x", "y"))
  )
}

construct_auto_label_labs <- function(plot_names, differential = FALSE) {
  if (length(plot_names) == 1)
    construct_auto_label_lab_set(construct_plot_label(plot_names, differential = differential))
  else rlang::exec(
    c,
    !!!purrr::map(plot_names, ~ construct_auto_label_lab_set(
      construct_plot_label(.x, differential),
      id_prefix = idize(.x)
    ))
  )
}

#' Create additional data description for some plots
#'
#' @noRd
cosntruct_uptake_plots_data_info <- function(differential) {
  if (differential) {
    "The table presents data from the chosen x plot range.
     The empty values (e.q. `Frac Diff DU`) mean there was not sufficient
     data for this peptide. There is a possibility that the measurement
     result is available for only one state of the peptide.
     Abbreviations from the table: Diff DU - differential deuterium uptake,
     Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
  } else {
    "The table presents data from the chosen x plot range.
     The empty values (e.q. `Frac DU`) means there was not sufficient data
     for this peptide. Abbreviations from the table: DU - deuterium uptake,
     Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
  }
}

#' Construct variable name
#'
#' @noRd
construct_var_name <- function(diff, theo, frac, var)
  paste0(
    "diff_" %.?% (diff),
    "theo_" %.?% (theo),
    "frac_" %.?% (frac),
    var
  )

#' Create html text with coloured cysteins
#'
#' @noRd
color_cysteins <- function(protein_sequence) {
  n <- nchar(protein_sequence)

  split <- first(strsplit(protein_sequence, ""))

  #TODO: I want this to be rendered as pre
  HTML(
    paste0(
      sapply(1:ceiling(n / 50), function(i) {
        line <- split[(50 * (i - 1) + 1):(50 * i)]
        line <- line[!is.na(line)]
        line[grepl("C", line)] <- "<font color='red'>C</font>"
        paste(line, collapse = "")
      }),
      collapse = "<br>"
    )
  )
}
