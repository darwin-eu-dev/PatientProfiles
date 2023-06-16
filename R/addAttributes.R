#' Get attributes from one cohort to another
#'
#' @param newcohort cohort to which to attach the attributes
#' @param oldcohort cohort from which to get the attributes
#'
#' @return new cohort with added attributes from the other given cohort
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockPatientProfiles()
#' attributes(cdm$cohort1)
#' x <- cdm$cohort1 %>%
#'   filter(cohort_definition_id == 1) %>%
#'   computeQuery()
#' attributes(x)
#' x <- addAttributes(x, cdm$cohort1)
#' attributes(cdm$cohort1)
#' }
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

  class(newcohort) <- class(oldcohort)

  return(newcohort)
}

#' Add cohort name for each cohort_definition_id
#'
#' @param cohort cohort to which add the cohort name
#'
#' @return cohort with an extra column with the cohort names
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addCohortName()
#' }
#'
addCohortName <- function(cohort) {
  cohort %>%
    dplyr::left_join(
      CDMConnector::cohortSet(cohort), by = "cohort_definition_id",
      copy = TRUE
    )
}

#' Add cdm name
#'
#' @param table Table in the cdm
#' @param cdm A cdm reference object
#'
#' @return Table with an extra column with the cdm names
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addCdmName()
#' }
#'
addCdmName <- function(table, cdm = NULL) {
  if (is.null(cdm)) {
    cdm <- attr(cdm, "cdm_reference")
  }
  table %>%
    dplyr::mutate(
      cdm_name = dplyr::coalesce(CDMConnector::cdmName(cdm), as.character(NA))
    )
}
