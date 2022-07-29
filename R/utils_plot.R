#' Custom ggplot2 theme for HaDeX
#'
#' @noRd
hadex_ggtheme <- function() {
  thm <- ggplot2::theme_bw()
  thm_sub <- ggplot2::theme(plot.background = ggplot2::element_rect(fill = NA, color = NA))
  thm[names(thm_sub)] <- thm_sub
  thm
}

#' Extract limits from reactive value containing range
#'
#' @noRd
extract_limits_from_range <- function(range) c(range()[[1]], range()[[2]])

#' Update plot axes and labels basing on reactive values
#'
#' @param plt evaluated plot
#' @param range_x reactive value with range of x axis, if NULL not changed
#' @param range_y reactive value with range of y axis, if NULL not changed
#' @param labels list of reactive values with labs and sizes for x, y axes and
#'     title
#' @param label_prefix if not NULL it contains name which should be used for
#'     wrapper to extract values from labels list; used if one tab contains
#'     multiple plots (e.g. Comparison and Woods)
#'
#' @noRd
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
