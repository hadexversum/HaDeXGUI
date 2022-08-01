#' mod_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_plot_ui <- function(
    id
    ##  other parameters can be included here, like differential (e.g. see chiclet)
){
  ns <- NS(id)

  ##  Most plot modules have fixed structure, which is described below
  ##  However, there are exceptions (see: plot_sequence_data)

  ##  This hadex_tab element corresponds to the split layout of plot: settings on the left,
  ##  plots and data on the right; if you want other layout, see examples of tab_page_*
  hadex_tab_plot(
    ##  Title of the tab displayed in the navbar
    ##  You can also use automatic construction:
    # construct_plot_label("Comparison and Woods", differential = FALSE),
    title = "FIXED TITLE",

    ##  Specification of left settings panel; it mainly contains the return of
    ##  install_settings_ui function:
    settings = install_settings_ui(
      names = c("calculation"),  # list of names of servers
      modes = c(
        ##  This parameter specifies any non-default modes used for given servers
        ##  like in the example commented out
        # state = "DOUBLE"  # use two states to select
      ),
      params = list(
        ##  This parameter should be use, if you have option for modifying ranges
        ##  or labels of the plot. For single plot you should use something like
        ##  below, possibly with specification of axes to modify; if you have
        ##  multiple plots in the tabs, see `mod_plot_comparison_and_woods` for
        ##  details
        # range_labs = construct_auto_range_labs("PLOT NAME"),
        # label_labs = construct_auto_label_labs("PLOT NAME")
      ),
      ns = ns
    ),

    ##  You can alternatively specify the settings panel with uis you want to
    ##  include manually, using hadex_panel_settings.
    ##  You should include list of settings uis that you want to include;
    ##  you can specify them one by one, separated by comma. Additionally,
    ##  you can use separators to have headers.
    ##  See `mod_comparison_and_woods_plot` as an example
    # settings = hadex_panel_settings(
    #   hadex_settings_separator("Some settings")
    #   mod_settings_calculation_ui(ns("calculation")),
    #   mod_settings_state_ui(ns("state"), mode = "MULTIPLE")
    # )

    ##  This parameter contains info about right side of the tab. Usually it is
    ##  simply a plot and corresponding data. You can also include multiple plots
    ##  (see plot_comparison_and_woods or plot_replicates) OR add some textual
    ##  output under the plot (see plot_volcano) OR add some button with special
    ##  stuff (see plot_uptake)
    display = mod_display_plot_ui(
      ns("display_plot"),
      plot_label = "PLOT LABEL",
    )
  )
}

#' mod_plot Server Functions
#'
#' @noRd
mod_mod_plot_server <- function(id, dat, params
                                ##  All plot tabs rely on dat and params parameters;
                                ##  dat contains loaded data, params include settings
                                ##  selected by user and basic data stats
                                ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### REACTIVES FOR DATA PROCESSING

    # this does not have to be called like that,
    # but it is a convention
    dat_processed <- reactive({
      data.frame(x = 1:5, y = 5:1)
    })

    ### OUT REACTIVES

    # names of reactives in this section are
    # fixed, if you want to use autoreturn;
    # if you want to have multiple plots, see
    # e.g. plot_comparison_and_woods

    plot_out <- reactive({
      plot(dat_processed())
    })

    dat_out <- reactive({
      dat_processed()
    })

    ### VALUES FOR RANGE AND LABEL SERVERS

    # if you use range and/or label server,
    # you should have range_specs/or and label_specs
    # list created here, see other plots for examples

    ### SERVER AND PLOT SETTINGS INVOCATION

    # Settings servers should be invoked here;
    # setting server called 'xyz' should have its results
    # assigned to reactive list called 's_xyz'; invoke_settings_servers
    # does that automatically' it also assigns proper values
    # to required parameters of the settings servers

    invoke_settings_servers(
      names = c("calculation")
    )

    # this part runs display server

    mod_display_plot_server("display_plot", plot_out, dat_out)

    ### RETURN OF THE PLOT AND DATA

    # autoreturn returns plot_out and data_out automatically;
    # if you have more than one plots in the tab, you need
    # to provide their names

    return(
      autoreturn()
    )
  })
}

## after creating the module, invokation of server should be
## added to app_server and ui should be inserted to app_ui
