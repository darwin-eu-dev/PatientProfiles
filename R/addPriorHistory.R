
#' Add a column to the current tibble with the prior history of the subject_id at a
#' certain date
#'
#' @param x cohort table to which add prior history to
#' @param cdm object containing the person table
#' @param priorHistoryAt name of the date field to use as date in table x
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return cohort table with added column containing prior history of the
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
#' result <- cdm$cohort1 %>% addPriorHistory(cdm)
#' }
addPriorHistory <- function(x,
                            cdm,
                            priorHistoryAt = "cohort_start_date",
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

  # check if ageAt length = 1 and is in table x
  checkmate::assertCharacter(priorHistoryAt, len = 1, add = errorMessage)

  priorHistoryExists <- priorHistoryAt %in% colnames(x)
  if (!isTRUE(priorHistoryExists)) {
    errorMessage$push("- priorHistoryAt is not found in x")
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

  #rename

  # rename so x contain subject_id

  if ("subject_id" %in% colnames(x) == FALSE) {
    x <- x %>%
      dplyr::rename("subject_id" = "person_id")
  } else {
    x <- x
  }


  x <- cdm[["observation_period"]] %>%
    dplyr::select("subject_id" = "person_id", "observation_period_start_date") %>%
    dplyr::inner_join(x %>%
                        dplyr::select("subject_id", dplyr::all_of(priorHistoryAt)) %>%
                        dplyr::distinct(),
                      by = "subject_id") %>%
    dplyr::mutate(
      prior_history = CDMConnector::datediff(start = "observation_period_start_date",
                                             end = !!priorHistoryAt)
    ) %>%
    dplyr::right_join(x,
                      by = c("subject_id", priorHistoryAt)) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "prior_history")
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
