
#' Add a column to the current tibble with the prior history of the subject_id at a
#' certain date
#'
#' @param x cohort table to which add prior history to
#' @param cdm object containing the person table
#' @param indexDate name of the date field to use as date in table x
#' @param priorHistoryName name of the new column to be added
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
                            indexDate = "cohort_start_date",
                            priorHistoryName = "prior_history",
                            tablePrefix = NULL) {

  x <- x %>%
    addDemographics(cdm = cdm,
                    indexDate = indexDate,
                    age = FALSE,
                    ageGroup = NULL,
                    ageDefaultDay = NULL,
                    ageDefaultMonth = NULL,
                    ageImposeDay =  FALSE,
                    ageImposeMonth = FALSE,
                    sex = FALSE,
                    priorHistory = TRUE,
                    priorHistoryName = priorHistoryName,
                    furureObservation = FALSE,
                    tablePrefix = tablePrefix
    )

  return(x)
}
