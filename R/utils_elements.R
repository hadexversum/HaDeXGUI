#' Elements of HaDeX GUI
#'
#' @noRd
hadex_tab_plot <- function(title, settings, display) tabPanel(
  title = title,
  fillRow(
    class = "HaDeX-plot-tab-content",
    flex = c(3, 7),
    settings,
    display
  )
)

hadex_tab_other <- function(title, ...) tabPanel(
  title = title,
  class = "HaDeX-nonplot-tab-content",
  ...
)

hadex_panel_settings <- function(...) wellPanel(
  class = "HaDeX-tab-content-element HaDeX-plot-settings-panel",
  fillCol(
    flex = NA,
    h4(
      class = "HaDeX-plot-settings-panels-header",
      "Select parameters for the plot"
    ),
    ...
  )
)

hadex_panel_display <- function(...) div(
  class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
  fillCol(
    class = "HaDeX-plot-display-panel-container",
    flex = NA,
    ...
  )
)

hadex_settings_separator <- function(title)
  h5(class = "HaDeX-plot-settings-panels-section-title", title)

hadex_panel_settings_section <- function(..., title = NULL) tagList(
  if (!is.null(title)) h5(class = "HaDeX-plot-settings-panels-section-title", title) else NULL,
  ...
)

#' Collapsible card
#'
#' @noRd
collapsible_card <- function(title, ..., init_collapsed = FALSE, id = NULL, fancy_icon = NULL) {
  obj_id <- if (!is.null(id)) id else gen_random_id("collapsible_")

  tagList(
    singleton(tags$head(tags$script(src = "utils/collapse.js", type = "text/javascript"))),
    singleton(tags$head(tags$link(href = "utils/collapse.css", rel = "stylesheet"))),

    div(
      h6(
        fancy_icon %?>% add_fancy_icon,
        p(title),
        icon(if (init_collapsed) "angle-down" else "angle-up", class = "arrow-icon"),
        class = if (init_collapsed) "card-header collapsed" else "card-header",
        `data-toggle` = "collapse",
        `data-target` = paste0("#", obj_id),
        `aria-expanded` = if (init_collapsed) "false" else "true"
      ),
      div(
        ...,
        id = obj_id,
        class = if (init_collapsed) "card-body collapse" else "card-body collapse in",
        `aria-expanded` = if (init_collapsed) "false" else "true"
      ),
      class = "collapsible-card"
    )
  )
}

#' Secondary button
#'
#' @noRd
secondary_button <- function(inputId, label, icon = NULL, width = NULL, ...)
  actionButton(inputId, label, icon, width, ..., class = "btn-secondary")
