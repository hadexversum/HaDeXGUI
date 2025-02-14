#' plot_coverage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_coverage_ui <- function(id){
  ns <- NS(id)

  hadex_tab_plot(
    title = "Coverage",

    settings = install_settings_ui(
      names = c("state", "subregion"),
      modes = c(
        state = "SINGLE"
      ),
      ns = ns
    ),
    display = mod_display_plot_ui(
      id = ns("display_plot"),
      plot_labels = c(
        peptide_coverage = "Peptide Coverage",
        position_frequency = "Postition Frequency"
      )
    )
  )
}

#' coverage Server Functions
#'
#' @noRd
mod_plot_coverage_server <- function(id, dat, params){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    dat_processed_position_frequency <- reactive({
      state <- s_state     %()% state
      HaDeX::create_overlap_distribution_dataset(
        dat(),
        protein          = params      %()% chosen_protein,
        state            = state,
        start            = s_subregion %()% start,
        end              = s_subregion %()% end,
        protein_sequence = params      %()% protein_sequence
      )
    })

    ### OUT REACTIVES

    plot_out_peptide_coverage <- reactive({
      HaDeX::plot_coverage(
        dat = dat(),
        protein = params  %()% chosen_protein,
        state   = s_state %()% state
      ) +
        coord_cartesian(xlim = c(
          s_subregion %()% start,
          s_subregion %()% end
        )) + #TODO: should we select subregion or update range?
        ggtitle(glue::glue("Peptide coverage for {params %()% chosen_protein} in {s_state %()% state}"))
    })

    plot_out_position_frequency <- reactive({
      HaDeX::plot_overlap_distribution(
        overlap_dist_dat = dat_processed_position_frequency(),
        start            = s_subregion %()% start,
        end              = s_subregion %()% end
      )
    })

    dat_out_peptide_coverage <- reactive({
      HaDeX::show_overlap_data(
        dat = dat(),
        protein = params      %()% chosen_protein,
        state   = s_state     %()% state,
        start   = s_subregion %()% start,
        end     = s_subregion %()% end
      )
    })

    dat_out_position_frequency <- reactive({

      dat_processed_position_frequency() %>%
        rename(
          Position        = "pos",
          `Amino acid`    = "amino",
          Coverage        = "coverage"
        )
    })

    ### SERVER AND PLOT SETTINGS INVOCATION

    invoke_settings_servers(
      names = c("state", "subregion"),
      modes = c(
        state = "SINGLE"
      )
    )

    mod_display_plot_server(
      id = "display_plot",
      plot_out = list(
        peptide_coverage = plot_out_peptide_coverage,
        position_frequency = plot_out_position_frequency
      ),
      dat_out = list(
        peptide_coverage = dat_out_peptide_coverage,
        position_frequency = dat_out_position_frequency
      )
    )

    ### RETURN OF THE PLOT AND DATA

    return(
      autoreturn("peptide_coverage", "position_frequency")
    )
  })
}
