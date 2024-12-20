#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)

  hadex_tab_other(
    title = "Report",

    wellPanel(
      hadex_panel_settings_section(
        title = "Select elements to include in the report",

        fluidRow(
          column(
            width = 6,
            purrr::imap(
              exportable_components,
              ~ checkboxInput(
                inputId = ns(glue::glue("include_{.y}_plot")),
                label = glue::glue("{.x} Plot"),
                value = FALSE
              )
            ),
            secondary_button(
              inputId = ns("select_all_plots"),
              label = "Select all plots"
            )
          ),
          column(
            width = 6,
            purrr::imap(
              exportable_components,
              ~ checkboxInput(
                inputId = ns(glue::glue("include_{.y}_dat")),
                label = glue::glue("{.x} Data"),
                value = FALSE
              )
            ),
            secondary_button(
              inputId = ns("select_all_data"),
              label = "Select all data"
            )
          )
        ),

        p("Elements chosen for report have the same parameters as chosen in suitable panels
          e.g. axis range, plot title or theoretical maximal exchange control.
          Adjust parameters as needed in the report."),

        downloadButton(
          outputId = ns("export"),
          label = "Create report",
          icon = icon("fas fa-download")
        )
      )
    )
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id,
                              params,
                              dat_export,
                              dat_summary,
                              input_info){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    selected_export_components <- reactive({

      # browser()

      ## checking the path in docker

      print("$$$$$$$$$$")
      print(getwd())
      print(app_sys("app/report_template.Rmd"))
      print("$$$$$$$$$$")

      purrr::imap_dfr(
        exportable_components,
        ~ data.frame(
          type = c("plot", "dat"),
          component_name = .y,
          nice_name = paste0(.x, c(" Plot", " Data"))
        )
      ) %>%
        mutate(input_id = glue::glue("include_{component_name}_{type}")) %>%
        rowwise() %>%
        mutate(include  = input[[input_id]]) %>%
        ungroup() %>%
        filter(include) %>%
        mutate(component_module_name = strsplit(component_name, split = "-"),
               has_subcomponents = (lengths(component_module_name) > 1)) %>%
        rowwise() %>%
        mutate(
          component_group = list(dat_export[[component_module_name[[1]]]][[type]]) ,
          component = if (has_subcomponents)
            list(component_group[[component_module_name[[2]]]]())
          else list(component_group())
          ) %>%
        pull(component, name = nice_name)
    })

    observe({
      for (name in names(exportable_components)) {
        updateCheckboxInput(
          session = session,
          inputId = glue::glue("include_{name}_plot"),
          value = TRUE
        )
      }
    }) %>% bindEvent(input[["select_all_plots"]])

    observe({
      for (name in names(exportable_components)) {
        updateCheckboxInput(
          session = session,
          inputId = glue::glue("include_{name}_dat"),
          value = TRUE
        )
      }
    }) %>% bindEvent(input[["select_all_data"]])

    output[["export"]] <- downloadHandler(
      filename = "HaDeX_Report.html",
      content = function(file) {
        rmarkdown::render(
          input = app_sys("app/report_template.Rmd"),
          # input = app_sys("/usr/local/lib/R/site-library/HaDeXGUI/app/report_template.Rmd"),
          output_file = file,
          quiet = FALSE,
          params = list(
            dat_params = params,
            dat_summary = dat_summary(),
            input_info = input_info(),
            components = selected_export_components()
          )
        )
      }
    )

    ### validator

    iv <- InputValidator$new()
    iv$add_rule("include_uptake_plot", compose_rules(
      ~ if (. && !(dat_export[["uptake"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Uptake Curve tab!",
    ))
    iv$add_rule("include_uptake_dat", compose_rules(
      ~ if (. && !(dat_export[["uptake"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Uptake Curve tab!",
    ))
    iv$add_rule("include_measurements-measurements_plot", compose_rules(
      ~ if (. && !(dat_export[["measurements"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Measurements tab!",
    ))
    iv$add_rule("include_measurements-measurements_dat", compose_rules(
      ~ if (. && !(dat_export[["measurements"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Measurements tab!",
    ))
    iv$add_rule("include_measurements-mass_uptake_plot", compose_rules(
      ~ if (. && !(dat_export[["measurements"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Measurements tab!",
    ))
    iv$add_rule("include_measurements-mass_uptake_dat", compose_rules(
      ~ if (. && !(dat_export[["measurements"]][["report_validation_peptide_selected"]]())) "You need to select a peptide in Measurements tab!",
    )) # rule for upladed structure for hires
    iv$enable()
  })
}

exportable_components <- c(
  # names scheme is as follows:
  # if given tab contains only one plot: {tab}
  # if given tab contains multiple plots: {tab}-{plot-specific name}
  `coverage-position_frequency` = "Position Frequency",
  `coverage-peptide_coverage` = "Peptide Coverage",
  `comparison_and_woods-comparison` = "Comparison",
  `comparison_and_woods-woods` = "Woods",
  uptake = "Uptake Curve",
  butterfly = "Butterfly",
  butterfly_diff = "Butterfly Differential",
  volcano = "Volcano",
  chiclet = "Chiclet",
  chiclet_diff = "Chiclet Differential",
  `measurements-measurements` = "Measurements",
  `measurements-mass_uptake` = "Measurements - Mass Uptake",
  `replicates-per_peptide_for_time` = "Replicates (for each peptide for selected timepoint)",
  `replicates-per_peptide_all_times` = "Replicates (for each pepetide split by timepoints)",
  `replicates-per_time` = "Replicates (for all peptides split by timepoints)",
  # hires str
  # hires diff str
  `hires_heatmap` = "High-resolution Uptake",
  `diff_hires_heatmap` = "High-resolution Differential Uptake"
)
