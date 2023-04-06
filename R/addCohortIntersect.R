#' It creates columns to indicate the presence of cohorts
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
#' @param indexDate date of reference in table x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#' @param flag TRUE or FALSE. If TRUE, flag will calculated for this
#' intersection
#' @param count TRUE or FALSE. If TRUE, the number of counts will be calculated
#' for this intersection
#' @param date TRUE or FALSE. If TRUE, date will be calculated for this
#' intersection
#' @param time TRUE or FALSE. If TRUE, time differebce will be calculated for
#' this intersection
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
#'   addCohortIntersect(
#'     cdm = cdm,
#'     targetCohortTable = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
addCohortIntersect <- function(x,
                               cdm,
                               targetCohortTable,
                               targetCohortId = NULL,
                               indexDate = "cohort_start_date",
                               targetStartDate = "cohort_start_date",
                               targetEndDate = "cohort_end_date",
                               window = list(c(0, Inf)),
                               flag = TRUE,
                               count = TRUE,
                               date = TRUE,
                               time = TRUE,
                               nameStyle = "{value}_{cohort_name}_{window_name}",
                               tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)
  checkmate::assertLogical(flag, any.missing = FALSE, len = 1)
  checkmate::assertLogical(count, any.missing = FALSE, len = 1)
  checkmate::assertLogical(date, any.missing = FALSE, len = 1)
  checkmate::assertLogical(time, any.missing = FALSE, len = 1)
  checkmate::assertTRUE(flag | count | date | time)
  value <- c("flag", "count", "date", "time")[c(flag, count, date, time)]

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = paramters$id_name,
      value = value,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}

#' It creates columns to indicate the presence of cohorts
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
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
#'   flagCohortPresence(
#'     cdm = cdm,
#'     targetCohortTable = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
flagCohortPresence <- function(x,
                               cdm,
                               targetCohortTable,
                               targetCohortId = NULL,
                               indexDate = "cohort_start_date",
                               targetStartDate = "cohort_start_date",
                               targetEndDate = "cohort_end_date",
                               window = list(c(0, Inf)),
                               nameStyle = "{cohort_name}_{window_name}",
                               tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = paramters$id_name,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}

#' It creates columns to indicate number of occurrences of intersection of two
#' cohorts
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
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
#' library(PatientProfiles)
#' library(dplyr)
#'
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
#'   countCohortOccurrences(
#'     cdm = cdm,
#'     targetCohortTable = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
countCohortOccurrences <- function(x,
                                   cdm,
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   window = list(c(0, Inf)),
                                   nameStyle = "{cohort_name}_{window_name}",
                                   tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = paramters$id_name,
      value = "count",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}

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
#' \donttest{
#' library(PatientProfiles)
#' library(dplyr)
#'
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
#'   timeToCohort(
#'     cdm = cdm,
#'     targetCohortTable = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
timeToCohort <- function(x,
                         cdm,
                         indexDate = "cohort_start_date",
                         targetCohortTable,
                         targetCohortId = NULL,
                         targetDate = "cohort_start_date",
                         order = "first",
                         window = c(0, Inf),
                         nameStyle = "{cohort_name}_{window_name}",
                         tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "time",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = paramters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}


#' Date of cohorts that are present in a certain window
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
#' \donttest{
#' library(PatientProfiles)
#' library(dplyr)
#'
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
#'   dateOfCohort(
#'     cdm = cdm,
#'     targetCohortTable = "cohort2"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
dateOfCohort <- function(x,
                         cdm,
                         indexDate = "cohort_start_date",
                         targetCohortTable,
                         targetCohortId = NULL,
                         targetDate = "cohort_start_date",
                         order = "first",
                         window = c(0, Inf),
                         nameStyle = "{cohort_name}_{window_name}",
                         tablePrefix = NULL) {
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "date",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = paramters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      tablePrefix = tablePrefix
    )

  return(x)
}
