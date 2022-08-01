#' This function is meant to be used with HaDeX read_hdx function to
#' catch errors
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_internal_messages <- function(expr){

  tryCatch({
    expr
  },
  error = function(e){
    validate(need(FALSE, conditionMessage(e)))
  })

}
