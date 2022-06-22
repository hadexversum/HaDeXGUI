#' settings_applying UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_applying_ui <- function(id){
  ns <- NS(id)
  HaDeX_plotSettingsSection(
    title = "Select the parameters:",

    uiOutput(ns("gen_chosen_protein")),
    uiOutput(ns("gen_chosen_control")),
    uiOutput(ns("gen_no_deut_control")),
    numericInput_h(inputId = ns("deut_part"),
                   label = "Choose D20 concentration [%]: ",
                   value = 90,
                   min = 0, max = 100, step = 1,
                   width = "100%"),

    fluidRow(
      column(
        width = 6,
        numericInput_h(inputId = ns("sequence_start_shift"),
                       label = "Sequence start:",
                       value = 1, step = 1, min = 1,
                       width = "100%"),
      ),
      column(
        width = 6,
        uiOutput(ns("gen_sequence_length"))
      )
    ),
    verbatimTextOutput(ns("sequence_length_exp_info"))
  )
}

#' settings_applying Server Functions
#'
#' @noRd
mod_settings_applying_server <- function(id, dat_adjusted){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ### reactive values

    proteins_from_file <- reactive({ unique(dat_adjusted()[["Protein"]]) })
    states_from_file <- reactive({ unique(dat_adjusted()[["State"]]) })
    file_has_modifications <- reactive({ attr(dat_adjusted(), "has_modification") })

    # TODO: should this value be calculated basing on already shifted values?
    max_range_from_file <- reactive({
      req(input[["chosen_protein"]])
      req(dat_adjusted())

      max(filter(dat_adjusted(), Protein == input[["chosen_protein"]])[['End']])
    })
    # TODO: should here dat_adjusted be used?
    times_from_file <- reactive({ sort(round(unique(dat_adjusted()[["Exposure"]]), digits = 3)) })

    # TODO: -\\-, i'm using times_from_file() bc of that
    times_with_control <- reactive({ setNames(times_from_file(), c(head(times_from_file(), -1), "chosen control")) })
    times_t <- reactive({ times_from_file()[times_from_file() > input[["no_deut_control"]] & times_from_file() < 99999] })
    max_range <- reactive({ max(max_range_from_file(), as.numeric(input[["sequence_length"]]), na.rm = TRUE) })

    states_chosen_protein <- reactive({
      req(input[["chosen_protein"]])
      dat_adjusted() %>%
        filter(Protein == input[["chosen_protein"]]) %>%
        select(State) %>%
        unique(.) %>%
        arrange(nchar(State)) %>%
        .[[1]]
    })

    dat <- reactive({
      req(
        input[["chosen_protein"]],
        input[["chosen_control"]]
      )

      dat_adjusted() %>%
        mutate(Start = Start + input[["sequence_start_shift"]] - 1,
               End = End + input[["sequence_start_shift"]] - 1) %>%
        # TODO: this function IS NOT exported from HaDeX due to empty line
        HaDeX:::create_control_dataset(control_protein = input[["chosen_protein"]],
                                       control_state = strsplit(input[["chosen_control"]], " \\| ")[[1]][2],
                                       control_exposure = strsplit(input[["chosen_control"]], " \\| ")[[1]][3])
    })

    ### ui outputs

    output[["gen_chosen_protein"]] <- renderUI({
      selectInput_h(inputId = ns("chosen_protein"),
                    label = "Choose protein: ",
                    choices = proteins_from_file(),
                    selected = proteins_from_file()[1],
                    width = "100%")
    })

    output[["gen_chosen_control"]] <- renderUI({
      req(input[["chosen_protein"]])

      options_for_control <- dat_adjusted() %>%
          filter(Protein == input[["chosen_protein"]]) %>%
          mutate(Exposure = round(Exposure, 4)) %>%
          select(Protein, State, Exposure) %>%
          arrange(State, desc(Exposure)) %>%
          unique(.) %>%
          mutate(control = paste0(Protein, " | ", State, " | ", Exposure)) %>%
          select(control)

      selectInput_h(inputId = ns("chosen_control"),
                    label = "Maximal exchange control: ",
                    choices = options_for_control,
                    width = "100%")
    })

    output[["gen_sequence_length"]] <- renderUI({
      numericInput_h(inputId = ns("sequence_length"),
                     label = "Sequence length:",
                     #TODO: is the hardcoded value relevant for ex. input?
                     value = max_range_from_file(), step = 1,
                     min = max_range_from_file(),
                     width = "100%")
    })

    output[["gen_no_deut_control"]] <- renderUI({
      req(times_from_file())
      no_deut_times <- times_from_file()[times_from_file() < 0.1]
      selectInput_h(inputId = ns("no_deut_control"),
                    label = "No deuterated time point:",
                    choices = no_deut_times,
                    selected = max(no_deut_times))
    })

    ### other outputs

    output[["sequence_length_exp_info"]] <- renderText({
      paste("Sequence length from the file is ", max_range_from_file(), ".")
    })

    ### validator

    iv <- InputValidator$new()
    iv$add_rule("sequence_start_shift", sv_gte(0))
    iv$add_rule("deut_part", sv_lte(100))
    iv$add_rule("sequence_length", compose_rules(
      ~ if (any(is.na(.))) "Must not contain `NA` values.",
      ~ if (. < max_range_from_file()) "Must be no shorther than any max end position in the file."
    ))
    iv$enable()

    ### return values

    # TODO: can those externally exposed values be renamed?
    return(
      list(
        proteins_from_file = proteins_from_file,
        states_from_file = states_from_file,
        file_has_modifications = file_has_modifications,
        times_from_file = times_from_file,
        times_t = times_t,
        times_with_control = times_with_control,
        max_range = max_range,
        states_chosen_protein = states_chosen_protein,
        dat = dat
      )
    )
  })
}
