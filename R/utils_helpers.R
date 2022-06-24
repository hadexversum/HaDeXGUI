#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom shinyhelper helper
#' @noRd
plotOutput_h <- function(outputId, ...)
  # TODO: restore spinner
  helper(plotOutput(outputId = outputId, ...),
         content = outputId, type = "markdown", buttonLabel = "Okay", easyClose = TRUE,
         icon = "far fa-question-circle", colour = "#856C9D")

## "DT::dataTableOutput"
dataTableOutput_h <- function(outputId, ...)
  helper(getFromNamespace("dataTableOutput", ns = "DT")(outputId = outputId, ...), content = outputId,
         type = "markdown", buttonLabel = "okay", easyClose = TRUE,
         icon = "far fa-question-circle", colour = "#856C9D")

func_vec <- c("selectInput", "textInput", "checkboxInput", "numericInput", "radioButtons", "checkboxGroupInput")
func_list <- setNames(lapply(func_vec, function(ith_fun)
  tmp_name <- function(inputId, ...) {
    helper(getFromNamespace(ith_fun, ns = "shiny")(inputId = inputId, ...),  content = inputId,
           type = "markdown", buttonLabel = "Okay", easyClose = TRUE,
           icon = "far fa-question-circle", colour = "#856C9D")
  }), func_vec)

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(names(func_list)[ith_fun_id], "_h"), value = func_list[[ith_fun_id]])
}

## "ggiraph::girafeOutput"
girafeOutput_h <- function(outputId, ...)
  helper(getFromNamespace("girafeOutput", ns = "ggiraph")(outputId = outputId, ...), content = outputId,
         type = "markdown", buttonLabel = "okay", easyClose = TRUE,
         icon = "far fa-question-circle", colour = "#856C9D")
