#' @importFrom ggplot2 theme_bw element_rect `%+replace%`
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


#' Get input as a reactive value
#'
#' @param name name of the input value
#'
#' @importFrom shiny getDefaultReactiveDomain reactive
#' @importFrom rlang expr
input_r <- function(name) {
  eval(rlang::expr(reactive({ input[[!!name]] }, env = parent.frame(3))))
}

update_axes_and_labels <- function(plt, zoom, labels) {
  plt +
    coord_cartesian(
      xlim = c(zoom[["x_range"]]()[[1]], zoom[["x_range"]]()[[2]]),
      ylim = c(zoom[["y_range"]]()[[1]], zoom[["y_range"]]()[[2]])
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
