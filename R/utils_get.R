#' Function to get the variable name for coverage heatmap plot
#'
#' @param type type of variable
#' @param theoretical [logical] indicator for theoretical controls
#' @param fractional [logical] indicator for fractional values
#'
#' @noRd
#'
#' @return Value name to be plotted on coverage heatmap plot
#'
get_coverage_heatmap_variable <- function(type,
                                          theoretical,
                                          fractional){

  type <- match.arg(type, c("AUC", "Back-exchange",
                    "Deuterium uptake", "Uncertainty(Deuterium uptake)",
                    "Differential deuterium uptake", "Uncertainty(Differential deuterium uptake)"))

  if(type == "AUC") return("auc")

  if(type == "Back-exchange") return("back_exchange")

  if(grepl("uptake", type)) { variable  <-  "deut_uptake" }

  if(fractional) { variable <-  paste0("frac_", variable) }

  if(theoretical) { variable  <-  paste0("theo_", variable) }

  if(grepl("Differential", type)) { variable  <-  paste0("diff_", variable) }

  if(grepl("Uncertainty", type)) { variable  <-  paste0("err_", variable) }

  return(variable)

}
