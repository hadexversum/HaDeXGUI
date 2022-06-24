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
