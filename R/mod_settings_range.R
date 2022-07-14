#' module stettings range UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_range_ui <- function(id, range_labs){
  ns <- NS(id)
  rlang::exec(
    collapsible_card,
    title = "Ranges",
    !!!lapply(seq_along(range_labs), function(i) {
      sliderInput(
        inputId = ns(names(range_labs)[i]),
        label = range_labs[i],
        min = 0,
        max = 1,
        value = c(0, 1),
        step = 1
      )
    }),
    init_collapsed = TRUE,
    fancy_icon = "arrows-alt-h"
  )
}

#' mod_settings_range Server Functions
#'
#' @noRd
mod_settings_range_server <- function(id,
                                      range_specs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    for (spec in range_specs) {
      rlang::inject(
        observe({
          res <- extract_range_spec(!!spec)

          updateSliderInput(
            session,
            inputId = res[["id"]],
            min = res[["min"]],
            max = res[["max"]],
            value = res[["value"]],
            step = res[["step"]]
          )
        })
      )
    }

    return(
      input_r_list(
        lapply(
          range_specs,
          function(spec) spec[["id"]]
        )
      )
    )
  })
}

range_spec <- function(expr, id,
                       env = parent.frame()) {
  list(
    id = id,
    expr_react = reactive(substitute(expr), quoted = TRUE, env = env)
  )
}

extract_range_spec <- function(spec) {
  spec_res <- spec[["expr_react"]]()

  value <- if ("value" %in% names(spec_res)) spec_res[["value"]] else c(spec_res[["min"]], spec_res[["max"]])
  step <- if ("step" %in% names(spec_res)) spec_res[["step"]] else 1

  list(
    id = spec[["id"]],
    min = spec_res[["min"]],
    max = spec_res[["max"]],
    value = value,
    step = step
  )
}
