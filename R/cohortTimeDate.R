
#' Time between cohorts
#'
#' @param x table containing the individuals for which the time to another
#' cohort will be added
#' @param cdm cdm containing the tables
#' @param indexDate Date of interest
#' @param targetCohortTable Cohort table to
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a time variable added for each
#' cohort of interest
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
timeToCohort <- function(x,
                         cdm,
                         indexDate = "cohort_start_date",
                         targetCohortTable,
                         targetCohortId = NULL,
                         targetDate = "cohort_start_date",
                         order = "first",
                         window = c(0, Inf),
                         nameStyle = "{value}_{id_name}_{window_name}",
                         tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkWindow(window)

  if (is.null(targetCohortId)) {
    targetCohortId <- CDMConnector::cohort_count(
      cdm[[targetCohortTable]]
    ) %>%
      dplyr::pull("cohort_definition_id")
  }

  cohortNames <- CDMConnector::cohort_set(
    cdm[[targetCohortTable]]
  ) %>%
    dplyr::filter(.data$cohort_definition_id %in%
      .env$targetCohortId) %>%
    dplyr::pull("cohort_name")

  x <- x %>%
    addIntersect(cdm,
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "time",
      filterVariable = "cohort_definition_id",
      filterId = targetCohortId,
      idName = cohortNames,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}


#' Date of cohorts
#'
#' @param x table containing the individuals for which the time to another
#' cohort will be added
#' @param cdm cdm containing the tables
#' @param indexDate Date of interest
#' @param targetCohortTable Cohort table to
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a time variable added for each
#' cohort of interest
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
dateOfCohort <- function(x,
                         cdm,
                         indexDate = "cohort_start_date",
                         targetCohortTable,
                         targetCohortId = NULL,
                         targetDate = "cohort_start_date",
                         order = "first",
                         window = c(0, Inf),
                         nameStyle = "{value}_{id_name}_{window_name}",
                         tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkWindow(window)

  if (is.null(targetCohortId)) {
    targetCohortId <- CDMConnector::cohort_count(
      cdm[[targetCohortTable]]
    ) %>%
      dplyr::pull("cohort_definition_id")
  }

  cohortNames <- CDMConnector::cohort_set(
    cdm[[targetCohortTable]]
  ) %>%
    dplyr::filter(.data$cohort_definition_id %in%
                    .env$targetCohortId) %>%
    dplyr::pull("cohort_name")

  x <- x %>%
    addIntersect(cdm,
                 tableName = targetCohortTable,
                 indexDate = indexDate,
                 value = "date",
                 filterVariable = "cohort_definition_id",
                 filterId = targetCohortId,
                 idName = cohortNames,
                 window = window,
                 targetStartDate = targetDate,
                 targetEndDate = NULL,
                 order = order,
                 nameStyle = nameStyle,
                 tablePrefix = tablePrefix
    )

  return(x)
}
