#' settings_label UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_label_ui <- function(id, label_labs){
  ns <- NS(id)

  collapsible_card(
    title = "Labels",
    fluidRow(
      column(
        width = 10,
        lapply(seq_along(label_labs), function(i) {
          textInput(
            inputId = ns(names(label_labs)[i]),
            label = label_labs[i],
            value = "" # updatable by observer
          )
        })
      ),
      column(
        width = 2,
        lapply(names(label_labs), function(lab) {
          numericInput_h(
            inputId = ns(paste0(lab, "_size")),
            label = "Size:",
            value = 15,
            min = 5
          )
        })
      )
    ),
    p("The axis ticks have the same size as the axis label.
      The legend text size is the same as the x axis label."),
    init_collapsed = TRUE,
    fancy_icon = "paragraph"
  )
}

#' settings_label Server Functions
#'
#' @noRd
mod_settings_label_server <- function(id, label_specs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    for (i in seq_along(label_specs)) {
      rlang::inject(
        observe({
          updateTextInput(
            session,
            inputId = names(label_specs)[!!i],
            value = label_specs[[!!i]]()
          )
        })
      )
    }

    return(
      input_r_list(
        c(
          names(label_specs),
          paste0(names(label_specs), "_size")
        )
      )
    )
  })
}

label_spec <- function(expr, env = parent.frame()) {
  if (is.reactive(expr)) expr
  else reactive(substitute(expr), quoted = TRUE, env = env)
}

