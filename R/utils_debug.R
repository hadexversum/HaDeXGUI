#' debug
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
