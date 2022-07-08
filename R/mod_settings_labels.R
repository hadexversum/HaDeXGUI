#' settings_labels UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_labels_ui <- function(id, plot_type, differential){
  ns <- NS(id)

  collapsible_card(
    title = "Adjust labels",
    fluidRow(
      column(
        width = 10,
        textInput(
          inputId = ns("title"),
          label = paste0(construct_plot_label(plot_type, differential), " title:"),
          value = "" # updatable by observer
        ),
        textInput(
          inputId = ns("x"),
          label = paste0(construct_plot_label(plot_type, differential)," axis x label:"),
          value = "" # updatable by observer
        ),
        textInput(
          inputId = ns("y"),
          label = paste0(construct_plot_label(plot_type, differential), " axis y label:"),
          value = "" # updatable by observer
        )
      ),
      column(
        width = 2,
        numericInput_h(
          inputId = ns("size_title"),
          label = "Size:",
          value = 15,
          min = 5
        ),
        numericInput_h(
          inputId = ns("size_x"),
          label = "Size:",
          value = 15,
          min = 5
        ),
        numericInput_h(
          inputId = ns("size_y"),
          label = "Size:",
          value = 15,
          min = 5
        )
      )
    ),
    p("The axis ticks have the same size as the axis label.
      The legend text size is the same as the x axis label."),
    init_collapsed = TRUE
  )
}

#' settings_labels Server Functions
#'
#' @noRd
mod_settings_labels_server <- function(id, label_specs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    for (spec in label_specs) {
      rlang::inject(
        observe({
          updateTextInput(
            session,
            inputId = (!!spec)[["id"]],
            value = (!!spec)[["expr_react"]]()
          )
        })
      )
    }

    return(
      input_r_list("title", "x", "y", "size_title", "size_x", "size_y")
    )
  })
}

label_spec <- function(expr, id,
                       env = parent.frame()) {
  expr_react <- if (is.reactive(expr)) expr
  else reactive(substitute(expr), quoted = TRUE, env = env)

  list(
    id = id,
    expr_react = expr_react
  )
}
