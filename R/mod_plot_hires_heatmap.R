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

  # 3dmol
  # tags$style(HTML("
  #     #toolbar {
  #       margin: 10px 0;
  #       position: relative !important;
  #       z-index: 1000;
  #     }
  #     #btn-screenshot {
  #       color: #FFFEFD;
  #       background: #485696 linear-gradient(180deg, #636fa5, #485696) repeat-x;
  #       border-color: #485696;
  #       white-space: normal;
  #     }
  #     #toggle-spin {
  #       cursor: pointer;
  #       margin-left: 20px;
  #       transform: scale(1.3);
  #       vertical-align: middle;
  #     }
  #   "))

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

    structure_color_map <- reactive({

      colors <- HaDeX::get_structure_color(aggregated_dat = dat_processed(),
                                 differential = differential,
                                 fractional = s_calculation[["fractional"]](),
                                 theoretical = s_calculation[["theoretical"]](),
                                 time_t = s_time[["t"]]())
    })

    ### STRUCTURE

    observeEvent(structure_path(), {
      validate(need(!is.null(structure_path()), "No structure file supplied. This can be done in the `Input data` tab."))

      # browser()

      color_map <- structure_color_map()
      names(color_map) <- 1L:length(structure_color_map())

      session$sendCustomMessage("renderStructure",
                                list(data = paste0(readLines(structure_path()), collapse = "\n"),
                                     colorMap = as.list(color_map),
                                     protName = tools::file_path_sans_ext(params[["chosen_protein"]]())
                                ))
    })


    ## r3dmol

    protein_structure <- reactive({

      validate(need(!is.null(structure_path()), "No PDB file supplied. This can be done in the `Input data` tab."))

      HaDeX::plot_aggregated_uptake_structure(
        aggregated_dat = dat_processed(),
        differential = differential,
        fractional = s_calculation[["fractional"]](),
        theoretical = s_calculation[["theoretical"]](),
        time_t = s_time[["t"]](),
        pdb_file_path = structure_path())

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

    mod_display_plot_structure_server("display_plot", plot_out, dat_out, structure_out)


    return(
      autoreturn()
    )
  })
}
