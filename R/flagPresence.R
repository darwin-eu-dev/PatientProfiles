#' It creates columns to indicate binary intersection of cohorts
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param tableName name of the cohort that we want to check for overlap
#' @param cohortId vector of cohort definition ids to include
#' @param indexDate date of reference in table x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return table with added columns with overlap information
#' @export
#'
#' @examples
#' \donttest{
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(1, 1, 1, 2, 2),
#'   cohort_start_date = as.Date(
#'     c(
#'       "2020-01-01",
#'       "2020-01-15",
#'       "2020-01-20",
#'       "2020-01-01",
#'       "2020-02-01"
#'     )
#'   ),
#'   cohort_end_date = as.Date(
#'     c(
#'       "2020-01-01",
#'       "2020-01-15",
#'       "2020-01-20",
#'       "2020-01-01",
#'       "2020-02-01"
#'     )
#'   )
#' )
#'
#' cohort2 <- dplyr::tibble(
#'   cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
#'   subject_id = c(1, 1, 1, 2, 2, 2, 1),
#'   cohort_start_date = as.Date(
#'     c(
#'       "2020-01-15",
#'       "2020-01-25",
#'       "2020-01-26",
#'       "2020-01-29",
#'       "2020-03-15",
#'       "2020-01-24",
#'       "2020-02-16"
#'     )
#'   ),
#'   cohort_end_date = as.Date(
#'     c(
#'       "2020-01-15",
#'       "2020-01-25",
#'       "2020-01-26",
#'       "2020-01-29",
#'       "2020-03-15",
#'       "2020-01-24",
#'       "2020-02-16"
#'     )
#'   ),
#' )
#'
#' cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)
#'
#' result <- cdm$cohort1 %>%
#'   flagPresence(
#'     cdm = cdm,
#'     tableName = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
flagPresence <- function(x,
                         cdm,
                         tableName,
                         cohortId = NULL,
                         indexDate = "cohort_start_date",
                         targetStartDate = "cohort_start_date",
                         targetEndDate = "cohort_end_date", # can be NULL
                         window = list(c(0, Inf)),
                         nameStyle = "{cohortName}_{window_name}",
                         tablePrefix = NULL) {

  # Checks done in the internal addIntersect function
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  if ("GeneratedCohortSet" %in% class(cdm[[tableName]]) & !is.null(cohortId)) {
    cohortId <- sort(cohortId)
    filterVariable <- "cohort_definition_id"
    if ("cohort_set" %in% names(attributes(cdm[[tableName]]))) {
      idName <- CDMConnector::cohortSet(cdm[[tableName]]) %>%
        dplyr::collect() %>%
        dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) %>%
        dplyr::arrange(.data$cohort_definition_id) %>%
        dplyr::pull("cohort_name")
    } else {
      idName <- paste0("cohort", cohortId)
    }
  } else {
    idName <- NULL
    filterVariable <- NULL
    cohortId <- NULL
  }
  nameStyle <- gsub("cohortName", "id_name", nameStyle)

  x <- x %>%
    addIntersect(
      cdm, tableName,
      filterVariable = filterVariable,
      filterId = cohortId, idName = idName, value = "flag",
      indexDate = indexDate, targetStartDate = targetStartDate,
      targetEndDate = targetEndDate, window = window,
      nameStyle = nameStyle, tablePrefix = tablePrefix
    ) %>%
    CDMConnector::computeQuery()

  return(x)
}
