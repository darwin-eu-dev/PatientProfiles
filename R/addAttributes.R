#' Get attributes from one cohort to another
#'
#' @param newcohort cohort to which to attach the attributes
#' @param oldcohort cohort from which to get the attributes
#'
#' @return new cohort with added attributes from the other given cohort
#' @export
#'
#' @examples
#'
addAttributes <- function(newcohort,
                          oldcohort) {

  if (!isTRUE(inherits(newcohort, "tbl_dbi"))) {
    cli::cli_abort("{newcohort} is not a valid table")
  }
  if (!isTRUE(inherits(oldcohort, "tbl_dbi"))) {
    cli::cli_abort("{oldcohort} is not a valid table")
  }

  for(at in names(attributes(oldcohort))) {
    if(is.null(attr(newcohort, at))) {
      attr(newcohort, at) <- attr(oldcohort, at)
    }
  }

  return(newcohort)
}
