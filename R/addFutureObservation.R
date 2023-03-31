
#' Add a column with the days of future observation for an individual
#'
#' @param x cohort table to which add prior history to
#' @param cdm object containing the person table
#' @param indexDate name of the date field to use as date in table x
#' @param futureObservationName name of the new column to be added
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return cohort table with added column containing future observation of the
#' individuals
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(CohortProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"),
#'     as.Date("2010-03-01"),
#'     as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"),
#'     as.Date("2013-01-01"),
#'     as.Date("2013-01-01")
#'   )
#' )
#'
#' obs_1 <- tibble::tibble(
#'   observation_period_id = c("1", "2", "3"),
#'   person_id = c("1", "2", "3"),
#'   observation_period_start_date = c(
#'     as.Date("2010-02-03"),
#'     as.Date("2010-02-01"),
#'     as.Date("2010-01-01")
#'   ),
#'   observation_period_end_date = c(
#'     as.Date("2014-01-01"),
#'     as.Date("2012-01-01"),
#'     as.Date("2012-01-01")
#'   )
#' )
#'
#' cdm <-
#'   mockCohortProfiles(
#'    seed = 1,
#'    cohort1 = cohort1,
#'     observation_period = obs_1
#'
#'   )
#'
#' result <- cdm$cohort1 %>% addFutureObservation(cdm)
#' }
addFutureObservation <- function(x,
                            cdm,
                            indexDate = "cohort_start_date",
                            futureObservationName = "future_observation",
                            tablePrefix = NULL) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push("- x is not a table")
  }

  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

  # check if indexDate length = 1 and is in table x
  checkmate::assertCharacter(indexDate, len = 1, add = errorMessage)
  checkmate::assertCharacter(futureObservationName, len = 1,
                             add = errorMessage,
  )
  priorHistoryExists <- indexDate %in% colnames(x)
  if (!isTRUE(priorHistoryExists)) {
    errorMessage$push("- indexDate is not found in x")
  }
#check cdm object
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push("- cdm must be a CDMConnector CDM reference object")
  }

  observationPeriodExists <- "observation_period" %in% names(cdm)
  if (!isTRUE(observationPeriodExists)) {
    errorMessage$push("- `observation_period` is not found in cdm")
  }

  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  x <- x %>%
    addDemographics(cdm = cdm,
                    indexDate = indexDate,
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = FALSE,
                    furureObservation = TRUE,
                    futureObservationName = futureObservationName,
                    tablePrefix = tablePrefix
    )

  return(x)
}
