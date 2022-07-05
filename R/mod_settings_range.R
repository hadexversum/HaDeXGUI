#' module stettings range UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_range_ui <- function(id, range_ids){
  ns <- NS(id)
  do.call(
    collapsible_card,
    args = c(
      title = "Ranges",
      lapply(range_ids, function(id) {
        sliderInput(
          inputId = ns(id),
          label = paste0("Choose ", id, " range for plot:"),
          min = 0,
          max = 1,
          value = c(0, 1),
          step = 1
        )
      }),
      init_collapsed = TRUE
    )
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
          range <- (!!spec)[["range_rv"]]()

          updateSliderInput(
            session,
            inputId = (!!spec)[["id"]],
            min = range[1],
            max = range[2],
            value = range,
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
    range_rv = reactive(substitute(expr), quoted = TRUE, env = env)
  )
}
