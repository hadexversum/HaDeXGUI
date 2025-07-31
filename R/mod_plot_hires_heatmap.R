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
    title = if(differential) "Differential heatmap + 3D Vis" else "Single heatmap + 3D Vis",

    settings = install_settings_ui(
      names = c("calculation", "state", "time"),
      modes = c(
        state = if (differential) "DOUBLE" else "SINGLE",
        time = "LIMITS AND EXPOSURE 2"
      ),
      ns = ns
    ),
    display = mod_display_plot_structure_ui(
      ns("display_plot"),
      plot_label = "Heatmap",
      structure = TRUE,
      additional_button_server = mod_export_hdxviewer_ui(ns("export_hdxviewer"))
        # downloadButton(ns("export_hdxviewer"), label = "Export to hdxViewer?")
    )
  )
}

#'
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

    ## colors for the structure

    structure_color_map <- reactive({


      colors <- HaDeX::get_structure_color(aggregated_dat = dat_processed(),
                                 differential = differential,
                                 fractional = s_calculation[["fractional"]](),
                                 theoretical = s_calculation[["theoretical"]](),
                                 time_t = s_time[["t"]]())

      names(colors) <- 1L:length(colors)

      colors
    })



    ## r3dmol

    # protein_structure <- reactive({
    #
    #   validate(need(!is.null(structure_path()), "No PDB file supplied. This can be done in the `Input data` tab."))
    #
    #   HaDeX::plot_aggregated_uptake_structure(
    #     aggregated_dat = dat_processed(),
    #     differential = differential,
    #     fractional = s_calculation[["fractional"]](),
    #     theoretical = s_calculation[["theoretical"]](),
    #     time_t = s_time[["t"]](),
    #     pdb_file_path = structure_path())
    #
    # })
    #
    # structure_out <- reactive({
    #
    #   protein_structure()
    #
    # })

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


    dat_out <- reactive({

      HaDeX::show_aggregated_uptake_data(
        dat_processed(),
        differential = differential,
        fractional = s_calculation[["fractional"]](),
        theoretical = s_calculation[["theoretical"]]()
      )

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
        time = "LIMITS AND EXPOSURE 2"
      )
    )

    ### HDX VIEWER EXPORT

    hdxviewer_dat <- reactive({

      HaDeX::prepare_hdxviewer_export(dat_processed(),
                                      differential = differential,
                                      theoretical = s_calculation[["theoretical"]](),
                                      fractional = s_calculation[["fractional"]]())
      })


    mod_export_hdxviewer_server(
       id = "export_hdxviewer",
       differential = differential,
       hdxviewer_dat = hdxviewer_dat, params, content = "hdxviewer"
       # dplyr::if_else(differential, paste0(s_state[["state_1"]](), "_", s_state[["state_2"]]()), s_state[["state"]]())
    )

    ### RETURN OF THE PLOT AND DATA

    mod_display_plot_structure_server("display_plot",
                                      plot_out = plot_out,
                                      dat_out = dat_out,
                                      structure_out = structure_out,
                                      file_path = structure_path,
                                      color_map = structure_color_map,
                                      chosen_protein = params[["chosen_protein"]]
                                      )


    return(
      autoreturn()
    )
  })
}
