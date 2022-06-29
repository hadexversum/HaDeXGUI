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
