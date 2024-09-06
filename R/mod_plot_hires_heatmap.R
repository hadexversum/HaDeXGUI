#' plot_chiclet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_hires_heatmap_ui <- function(id, differential = FALSE){
  ns <- NS(id)

  hadex_tab_plot(
    title = if(differential) "Differential heatmap" else "Single heatmap",

    settings = install_settings_ui(
      names = c("calculation", "state", "time"),
      modes = c(
        state = if (differential) "DOUBLE" else "SINGLE",
        time = "LIMITS AND EXPOSURE"
      ),
      ns = ns
    ),
    display = mod_display_plot_structure_ui(
      ns("display_plot"),
      plot_label = "Heatmap",
      structure = TRUE,
      additional_button_below = downloadButton(ns("export_hdxviewer"),
                                             label = "Export to hdxViewer?")
    )
  )
}

#' plot_chiclet Server Functions
#'
#' @noRd
mod_plot_hires_heatmap_server <- function(id, dat, params, structure_path, differential = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING


    dat_processed <-  if(differential) reactive({

      dat() %>%
        HaDeX::create_diff_uptake_dataset(
          protein = params[["chosen_protein"]](),
          state_1 = s_state[["state_1"]](),
          state_2 = s_state[["state_2"]](),
          time_0 = s_time[["0"]](),
          time_100 = s_time[["100"]](),
          deut_part = params[["deut_part"]]()
        ) %>%
        HaDeX::create_aggregated_diff_uptake_dataset()

    }) else reactive({

      dat() %>%
        HaDeX::create_uptake_dataset(
          protein = params[["chosen_protein"]](),
          states = s_state[["state"]](),
          time_0 = s_time[["0"]](),
          time_100 = s_time[["100"]](),
          deut_part = params[["deut_part"]]()
        ) %>%
        HaDeX::create_aggregated_uptake_dataset()
    })

    ### STRUCTURE


    protein_structure <- reactive({

      validate(need(!is.null(structure_path()), "No PDB file supplied."))

      HaDeX::plot_aggregated_uptake_structure(
        aggregated_dat = dat_processed(),
        differential = differential,
        fractional = s_calculation[["fractional"]](),
        theoretical = s_calculation[["theoretical"]](),
        time_t = s_time[["t"]](),
        pdb_file_path = structure_path()
        )

    })

    structure_out <- reactive({

      protein_structure()

    })

    ### OUT REACTIVES

    plot_out <- if(differential) reactive({

     (dat_processed() %>%
        HaDeX::plot_aggregated_differential_uptake(panels = F,
                                                   fractional = s_calculation[["fractional"]](),
                                                   theoretical = s_calculation[["theoretical"]](),
                                                   interactive = T)
      ) %>% suppressMessages()

    }) else reactive({
      (dat_processed() %>%
         HaDeX::plot_aggregated_uptake(panels = F,
                                       fractional = s_calculation[["fractional"]](),
                                       theoretical = s_calculation[["theoretical"]](),
                                       interactive = T)
      )  %>%
        suppressMessages()
    })

    .show_fun <- if (differential) HaDeX::show_diff_uptake_data else HaDeX::show_uptake_data

    dat_out <- reactive({
      dat_processed()
      # %>% # TODO: prepare for this format
      #   .show_fun(
      #     theoretical = s_calculation[["theoretical"]](),
      #     fractional = s_calculation[["fractional"]]()
      #   )

    })


    label_specs <- list(
      title = label_spec("Test"),
      y = label_spec("Exposure [min]"),
      x = label_spec("Peptide ID")
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    invoke_settings_servers(
      names = c(
        "calculation", "state", "time"
      ),
      modes = c(
        state = if(differential) "DOUBLE" else "SINGLE",
        time = "LIMITS AND EXPOSURE"
      )
    )

    ### HDX VIEWER EXPORT

    output[["export_hdxviewer"]] <- downloadHandler(
      paste0(id, ".csv"),
      content = function(file) {
        HaDeX::prepare_hdxviewer_export(dat_processed(),
                                        differential = differential,
                                        theoretical = s_calculation[["theoretical"]](),
                                        fractional = s_calculation[["fractional"]]()) %>%
          write.csv(., file, row.names = FALSE, quote=FALSE)

      }
    )

    ### RETURN OF THE PLOT AND DATA

    mod_display_plot_structure_server("display_plot", plot_out, dat_out, structure_out)


    return(
      autoreturn()
    )
  })
}
