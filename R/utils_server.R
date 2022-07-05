#' @importFrom ggplot2 theme_bw theme element_rect %+replace%
HaDeX_ggtheme <- theme_bw() %+replace%
  theme(plot.background = element_rect(fill = NA, color = NA))

#' @importFrom icecream ic_enable
#' @importFrom withr with_options
#' @importFrom ggplot2 theme_set
apply_server_settings <- function() {
  if (getOption("golem.app.prod")) {
    ic_enable()
    options(icecream.always.include.context = TRUE)
  }

  theme_set(HaDeX_ggtheme)
}

extract_limits_from_range <- function(range) c(range()[[1]], range()[[2]])

update_axes_and_labels <- function(plt, range_x = NULL, range_y = NULL, labels) {
  plt +
    coord_cartesian(
      xlim = ic(range_x %?>% extract_limits_from_range),
      ylim = ic(range_y %?>% extract_limits_from_range)
    ) +
    labs(
      title = labels[["title"]](),
      x = labels[["x_lab"]](),
      y = labels[["y_lab"]]()
    ) +
    theme(
      plot.title = element_text(size = labels[["title_size"]]()),
      axis.text.x = element_text(size = labels[["x_lab_size"]]()),
      axis.title.x = element_text(size = labels[["x_lab_size"]]()),
      axis.title.y = element_text(size = labels[["y_lab_size"]]()),
      axis.text.y = element_text(size = labels[["y_lab_size"]]()),
      legend.text = element_text(size = labels[["x_lab_size"]]()),
      legend.title = element_text(size = labels[["x_lab_size"]]())
    )
}

toggle_id <- function(condition, id) {
  golem::invoke_js(
    if (condition) "showid" else "hideid",
    id
  )
}
