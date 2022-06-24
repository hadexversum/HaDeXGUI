#' uptake_butterfly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_uptake_butterfly_ui <- function(id) {
  ns <- NS(id)
  HaDeX_plotTab(
    title = "Butterfly Plot",
    settingsPanel = HaDeX_plotSettingsPanel(
      butterfly_general_settings(ns),
      butterfly_state(ns),
      butterfly_timepoints(ns),
      butterfly_visualization(ns),
      butterfly_zoom(ns),
      butterfly_labels_adjustement(ns)
    ),
    displayPanel = HaDeX_plotDisplayPanel(
      butterfly_plot_panel(ns)
    )
  )
}

butterfly_general_settings <- function(ns) HaDeX_plotSettingsSection(
  checkboxInput_h(inputId = ns("theoretical"),
                  label = "Theoretical calculations",
                  value = FALSE),
  checkboxInput_h(inputId = ns("fractional"),
                  label = "Fractional values",
                  value = FALSE)
)

butterfly_state <- function(ns) HaDeX_plotSettingsSection(
  title = "State",
  uiOutput(ns("gen_state"))
)

butterfly_timepoints <- function(ns) HaDeX_plotSettingsSection(
  title = "Timepoints",
  fluidRow(
    column(width = 6,
           uiOutput(ns("gen_timepoints"))
    ),
    column(width = 6,
           uiOutput(ns("gen_time_0")),
           uiOutput(ns("gen_time_100"))
    )
  )
)

butterfly_visualization <- function(ns) HaDeX_plotSettingsSection(
  title = "Visualization",
  selectInput_h(inputId = ns("uncertainty"),
                label = "Show uncertainty as:",
                choices = c("ribbon", "bars", "bars + line"),
                selected = "ribbon")
)

butterfly_zoom <- function(ns) HaDeX_plotSettingsSection(
  title = "Zoom",
  uiOutput(ns("gen_x_range")),
  uiOutput(ns("gen_y_range"))
)

butterfly_labels_adjustement <- function(ns) HaDeX_plotSettingsSection(
  HaDeX_collapseButton(
    title = "Adjust labels",
    target = "#HaDeX-butterfly-labels-adjusting-panel"
  ),

  HaDeX_collapsablePanel(
    id = "HaDeX-butterfly-labels-adjusting-panel",
    fluidRow(
      column(width = 10,
             uiOutput(ns("gen_plot_title")),
             textInput(inputId = ns("plot_x_label"),
                       label = "Butterfly plot axis x label:",
                       value = "Peptide ID"),
             uiOutput(ns("gen_plot_y_label"))),
      column(width = 2,
             numericInput_h(inputId = ns("plot_title_size"),
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = ns("plot_x_label_size"),
                            label = "Size:",
                            value = 15,
                            min = 5),
             numericInput_h(inputId = ns("plot_y_label_size"),
                            label = "Size:",
                            value = 15,
                            min = 5))
    ),
    p("The axis ticks have the same size as the axis label.
      The legend text size is the same as the x axis label.")
  )
)

butterfly_plot_panel <- function(ns) tabsetPanel(
  tabPanel("Butterfly plot",
           girafeOutput_h(ns("plot")),
           downloadButton(ns("plot_download_button"),
                          "Save chart (.svg)")),
  tabPanel("Data",
           DT::dataTableOutput(ns("plot_data")),
           p(
             "The empty values (e.q. `Frac DU`) means there was not sufficient data for this peptide.",
             "Abbreviations from the table: DU - deuterium uptake, Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
           )
  )
)

#' uptake_butterfly Server Functions
#'
#' @import ggplot2
#' @noRd
mod_uptake_butterfly_server <- function(
    id, dat,
    chosen_protein, states_chosen_protein, times_from_file, times_with_control,
    deut_part, no_deut_control){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dat_plot <- reactive({
      # TODO: check which validates are really needed
      validate(need(chosen_protein() %in% unique(dat()[["Protein"]]),
                    "Wait for the parameters to be loaded."))
      validate(need(input[["state"]] %in% states_chosen_protein(),
                    "Wait for the parameters to be loaded."))

      HaDeX::create_state_uptake_dataset(
        dat(),
        protein = chosen_protein(),
        state = input[["state"]],
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        deut_part = deut_part() / 100
      )
    })

    plot_obj <- reactive({
      dat_plot() %>%
        filter(Exposure %in% input[["timepoints"]]) %>%
        HaDeX::plot_butterfly(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]],
          uncertainty_type = input[["uncertainty"]]
        ) + geom_point_interactive(
          aes(tooltip = paste0(
            Sequence,
            "<br/>Position: ", Start, "-", End,
            "<br/>Value: ", round(value, 2),
            "<br/> Exposure: ", Exposure
          ))
        )
    })

    plot_out <- reactive({
      plot_obj() +
        coord_cartesian(
          xlim = c(input[["x_range"]][[1]], input[["x_range"]][[2]]),
          ylim = c(input[["y_range"]][[1]], input[["y_range"]][[2]])) +
        labs(
          title = input[["plot_title"]],
          x = input[["plot_x_label"]],
          y = input[["plot_y_label"]]) +
        theme(
          plot.title = element_text(size = input[["plot_title_size"]]),
          axis.text.x = element_text(size = input[["plot_x_label_size"]]),
          axis.title.x = element_text(size = input[["plot_x_label_size"]]),
          axis.title.y = element_text(size = input[["plot_y_label_size"]]),
          axis.text.y = element_text(size = input[["plot_y_label_size"]]),
          legend.text = element_text(size = input[["plot_x_label_size"]]),
          legend.title = element_text(size = input[["plot_x_label_size"]])
        )
    })

    dat_out <- reactive({
      dat_plot() %>%
        HaDeX::show_uptake_data(
          theoretical = input[["theoretical"]],
          fractional = input[["fractional"]]
        ) %>%
        filter(Exposure %in% input[["timepoints"]]) %>%
        filter(ID >= input[["x_range"]][[1]] &
               ID <= input[["x_range"]][[2]])
    })

    output[["gen_state"]] <- renderUI({
      selectInput_h(
        inputId = ns("state"),
        label = "Choose state:",
        choices = states_chosen_protein(),
        selected = states_chosen_protein()[1]
      )
    })

    output[["gen_time_0"]] <- renderUI({
      selectInput_h(
        inputId = ns("time_0"),
        label = "Deut 0%",
        choices = times_from_file()[times_from_file() < 99999],
        selected = times_from_file()[times_from_file() == no_deut_control()]
      ) %visible if% !input[["theoretical"]]
    })

    output[["gen_time_100"]] <- renderUI({
      selectInput_h(
        inputId = ns("time_100"),
        label = "Deut 100%",
        choices = times_with_control(),
        selected = max(times_with_control()[times_with_control() < 99999])
      ) %visible if% (!input[["theoretical"]] && input[["fractional"]])
    })

    output[["gen_timepoints"]] <- renderUI({
      vec <- if (input[["fractional"]])
        times_from_file() < as.numeric(input[["time_100"]])
      else
        times_from_file() < 99999

      times_t <- times_from_file()[times_from_file() > input[["time_0"]] & vec]

      checkboxGroupInput_h(
        inputId = ns("timepoints"),
        label = "Show time points: ",
        choices = times_t,
        selected = times_t
      )
    })

    output[["gen_x_range"]] <- renderUI({
      max_x <- max(dat_plot()[["ID"]])
      min_x <- min(dat_plot()[["ID"]])

      sliderInput(
        inputId = ns("x_range"),
        label = "Choose x range for butterfly plot:",
        min = min_x,
        max = max_x,
        value = c(min_x, max_x),
        step = 1
      )
    })

    output[["gen_y_range"]] <- renderUI({
      if (input[["fractional"]]){
        max_y <- ceiling(max(dat_plot()[["frac_deut_uptake"]], dat_plot()[["theo_frac_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_plot()[["frac_deut_uptake"]], dat_plot()[["theo_frac_deut_uptake"]], na.rm = TRUE)) - 1
      } else {
        max_y <- ceiling(max(dat_plot()[["deut_uptake"]], dat_plot()[["theo_deut_uptake"]], na.rm = TRUE)) + 1
        min_y <- floor(min(dat_plot()[["deut_uptake"]], dat_plot()[["theo_deut_uptake"]], na.rm = TRUE)) - 1
      }

      sliderInput(
        inputId = ns("y_range"),
        label = "Choose y range for butterfly plot:",
        min = min_y,
        max = max_y,
        value = c(min_y, max_y),
        step = 1
      )
    })

    output[["gen_plot_title"]] <- renderUI({
      textInput(
        inputId = ns("plot_title"),
        label = "Butterfly plot title:",
        value = if (input[["theoretical"]])
          paste0("Theoreotical butterfly plot for ", input[["state"]], " state for ", chosen_protein())
        else
          paste0("Butterfly plot for ", input[["state"]], " state for ", chosen_protein())
      )
    })

    output[["gen_plot_y_label"]] <- renderUI({
      textInput(
        inputId = ns("plot_y_label"),
        label = "Butterfly plot axis y label:",
        value = if (input[["fractional"]])
          "Fractional deuterium uptake [%]"
        else
          "Deuterium uptake [Da]"
      )
    })

    output[["plot"]] <- renderGirafe({ girafe(ggobj = plot_out(), width_svg = 12, height_svg = 7) })

    output[["data"]] <- renderDataTable({ HaDeX_DT_format(dat_out()) })

    output[["plot_download_button"]] <- downloadHandler(
      "butterfly_plot.svg",
      content = function(file) {
        ggsave(file, plot_out(), device = svg,
               height = 300, width = 400, units = "mm")
      })

  })
}

## To be copied in the UI
# mod_uptake_butterfly_ui("uptake_butterfly_1")

## To be copied in the server
# mod_uptake_butterfly_server("uptake_butterfly_1")
