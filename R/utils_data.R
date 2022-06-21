#' @importFrom xtable xtable
htmlize_data <- function(dataset_name) {
  data("file_req", package = "HaDeXGUI", envir = environment())
  HTML(paste(capture.output(print(xtable(file_req), type = "html"))))
}
