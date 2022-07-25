#' settings_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_variable_ui <- function(id){
  ns <- NS(id)
  collapsible_card(
    title = "Variable",
    selectizeInput_h(
      inputId = ns("variable"),
      label = "Select variable to map for color:",
      choices = c(
        "back_exchange",
        "theo_frac_deut_uptake",
        "deut_uptake",
        "frac_deut_uptake",
        "theo_deut_uptake",
        "diff_frac_deut_uptake",
        "diff_deut_uptake",
        "diff_theo_frac_deut_uptake",
        "diff_theo_deut_uptake",
        "err_frac_deut_uptake",
        "err_deut_uptake",
        "err_theo_frac_deut_uptake",
        "err_theo_deut_uptake",
        "err_diff_frac_deut_uptake",
        "err_diff_deut_uptake",
        "err_diff_theo_frac_deut_uptake",
        "err_diff_theo_deut_uptake",
        "auc"
      ),
      selected = "frac_deut_uptake"
    ),
    fancy_icon = "table"
  )
}

#' settings_variable Server Functions
#'
#' @noRd
mod_settings_variable_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      input_r_list("variable")
    )
  })
}
