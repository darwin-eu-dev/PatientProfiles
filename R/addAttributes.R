
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
      CDMConnector::cohortSet(cohort) %>%
        dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_definition_id",
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
  table %>%
    dplyr::mutate(cdm_name = dplyr::coalesce(
      CDMConnector::cdmName(cdm %||% attr(table, "cdm_reference")),
      as.character(NA)
    ))
}
