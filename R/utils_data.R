#' @importFrom xtable xtable
htmlize_data <- function(dataset_name) {
  data("file_req", package = "HaDeXGUI", envir = environment())
  HTML(paste(capture.output(print(xtable(file_req), type = "html"))))
}


HaDeX_DT_format <- function(dat, cols = colnames(dat)) {
  DT::datatable(
    data = dat,
    colnames = cols,
    class = "table-bordered table-condensed",
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      dom = "tBip",
      autoWidth = TRUE,
      buttons = c("excel", "pdf")
    ),
    filter = "bottom",
    rownames = FALSE
  )
}

MAX_TIME <- 99999

cosntruct_uptake_plots_data_info <- function(differential) {
  if (differential) {
    "The table presents data from the chosen x plot range.
     The empty values (e.q. `Frac Diff DU`) mean there was not sufficient
     data for this peptide. There is a possibility that the measurement
     result is available for only one state of the peptide.
     Abbreviations from the table: Diff DU - differential deuterium uptake,
     Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
  } else {
    "The table presents data from the chosen x plot range.
     The empty values (e.q. `Frac DU`) means there was not sufficient data
     for this peptide. Abbreviations from the table: DU - deuterium uptake,
     Frac - fractional, Theo - theoretical, U(value) - uncertainty of value."
  }
}

first <- function(x) x[1]
last <- function(x) x[length(x)]
middle <- function(x) x[ceiling(length(x) / 2)]



