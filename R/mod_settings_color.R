#' settings_color UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_color_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Colors",
    uiOutput_h(ns("gen_state_color_pickers")),
    init_collapsed = TRUE,
    fancy_icon = "palette"
  )
}

get_default_colors <- function(n)
  hcl.colors(n, palette = "Set 2", alpha = NULL, rev = FALSE, fixup = TRUE)

#' settings_color Server Functions
#'
#' @noRd
mod_settings_color_server <- function(id, s_state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output[["gen_state_color_pickers"]] <- renderUI({
      states <- s_state %()% states
      colors <- get_default_colors(length(states))

      rlang::exec(
        tagList,
        !!!mapply(function(state, color) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_", gsub(" ", "", state))),
            label = paste0("Color for ", state, " state"),
            value = color)
        }, states, colors, SIMPLIFY = FALSE)
      )
    })
    outputOptions(output, "gen_state_color_pickers", suspendWhenHidden = FALSE)

    return(
      list(
        values = reactive({
          states <- s_state %()% states

          colors <- sapply(
            paste0("color_", states),
            function(x) input[[gsub(" ", "", x)]]
          )

          wait_for(length(colors) > 0)
          wait_for(all(sapply(colors, not_null)))

          setNames(colors, states)
        })
      )
    )
  })
}
