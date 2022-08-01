#' plot_sequence_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_sequence_data_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = "Sequence Data",
    settings = hadex_panel_settings(
      mod_settings_visualization_ui(ns("visualization"), mode = "SEQUENCE DATA")
    ),
    display = div(
      class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
      fillCol(
        class = "HaDeX-plot-display-panel-container",
        flex = NA,
        h5("Protein info: "),
        dataTableOutput_h(ns("protein_info")),
        h5("Reconstructed sequence: "),
        uiOutput(ns("colored_sequence")),
        girafeOutput_h(ns("plot_composition")),
        downloadButton(ns("plot_download_button"), "Save chart (.svg)")
      )
    )
  )
}

#' plot_sequence_data Server Functions
#'
#' @noRd
mod_plot_sequence_data_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed <- reactive({
      # TODO: this should be in HaDeX, not in GUI
      dat() %>%
        filter(Protein == params %()% chosen_protein) %>%
        select(Start, End, Sequence) %>%
        unique(.) %>%
        apply(1, function(x) data.frame(position = x[1]:x[2], amino = strsplit(x[3], '')[[1]], stringsAsFactors = FALSE)) %>%
        bind_rows() %>%
        unique(.) %>%
        left_join(amino_prop)
    })

    plot_out <- reactive({
        HaDeX::plot_amino_distribution(
          dat_processed(),
          hydro_properties = switch(
            s_visualization %()% show_residues,
            `Hydrophilic and hydrophobic` = c("phobic", "philic"),
            `Only hydrophobic` = "phobic",
            `Only hydrophilic` = "philic"
          ),
          protein = params %()% chosen_protein,
          charge_colors = c("-1" = "#E41A1C", "0" = "#377EB8", "1" = "#4DAF4A")
        )
    })

    ### CUSTOM OUTPUTS

    # here, instead of creating specified module,
    # everything is contained here as it is unique to
    # this tab

    output[["protein_info"]] <- DT::renderDataTable({
      c(
        name     = params %()% chosen_protein,
        length   = params %()% sequence_length, # TODO: should there be max_range?
        coverage = HaDeX::get_protein_coverage(
          dat            = dat(),
          protein        = params %()% chosen_protein,
          protein_length = params %()% max_range
        ) %>%
          paste0(., "%"),
        cysteins = length(gregexpr("C", params %()% protein_sequence)[[1]])
      ) %>%
        data.frame(
          Property = c("Protein name", "Length", "Coverage", "Number of cysteins"),
          Value = .
        ) %>%
        hadex_datatable(dom = "t")
    })

    output[["colored_sequence"]] <- renderUI({
      color_cysteins(params %()% protein_sequence)
    })

    output[["plot_composition"]] <- renderGirafe({
      girafe(ggobj = plot_out())
    })

    output[["plot_download_button"]] <- downloadHandler(
      paste0(id, ".svg"),
      content = function(file) {
        ggsave(file, plot_out(), device = svg,
               height = 300, width = 400, units = "mm")
      }
    )

    ### SERVER AND PLOT SETTINGS INVOCATION

    s_visualization <- mod_settings_visualization_server("visualization", mode = "SEQUENCE DATA")
  })
}
