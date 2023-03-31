
#' Add a column with the days of future observation for an individual
#'
#' @param x cohort table to which add prior history to
#' @param cdm object containing the person table
#' @param indexDate name of the date field to use as date in table x
#' @param name name of the new column to be added
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
                            name = "future_observation",
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
  checkmate::assertCharacter(name, len = 1,
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

  # rename so x contain subject_id
  xType <- dplyr::if_else("person_id" %in% names(x),
                          "cdm_table", "cohort")

  if(xType == "cdm_table"){
    x <- x %>%
      dplyr::rename("subject_id" = "person_id")
  }

  # add current observation period
  x <- x %>%
    dplyr::left_join(x %>%
                       dplyr::select("subject_id",
                                     indexDate) %>%
                       dplyr::inner_join(cdm[["observation_period"]]  %>%
                                           dplyr::rename("subject_id"="person_id") %>%
                                           dplyr::select("subject_id",
                                                         "observation_period_start_date",
                                                         "observation_period_end_date"),
                                         by = "subject_id") %>%
                       dplyr::filter(.data$observation_period_start_date <=
                                       !!rlang::sym(indexDate) &
                                       .data$observation_period_end_date >=
                                       !!rlang::sym(indexDate)) %>%
                       dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", indexDate)))) %>%
                       dplyr::summarise(observation_period_start_date =
                                          max(.data$observation_period_start_date, na.rm = TRUE),
                                        observation_period_end_date =
                                          max(.data$observation_period_end_date, na.rm = TRUE)) %>%
                       dplyr::select(!indexDate) %>%
                       dplyr::distinct(),
                     by = "subject_id")

  x <- x %>%
   dplyr::mutate(!!!futureObservationQuery(indexDate, name = name)) %>%
   dplyr::select(!c("observation_period_start_date",
                    "observation_period_end_date"))

  if(xType == "cdm_table"){
    x <- x %>%
      dplyr::rename("person_id" = "subject_id")
  }

  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)
}
