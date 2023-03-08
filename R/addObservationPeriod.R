# This file is part of CohortProfiles

#' Add a column to the current tibble with the observation period from observation_period table
#'
#'
#' @param x cohort table to which add prior history to
#' @param cdm object containing the person table
#' @param observationAt name of the date field to use as date in table x
#' @param column name of the observational start date and end date in the observation_period table
#' @param name name of the columns added for observation start
#' date and end date in form of "name for start date","name for end date"
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return table with added columns with obersvation information
#' @export
#'
#' @examples
#' \dontrun{
#' #create mock tables for testing
#'cohort1 <- tibble::tibble(
#'  cohort_definition_id = c("1", "1", "1"),
#'  subject_id = c("1", "2", "3"),
#'  cohort_start_date = c(
#'    as.Date("2010-03-03"),
#'    as.Date("2010-03-01"),
#'    as.Date("2010-02-01")
#'  ),
#'  cohort_end_date = c(
#'    as.Date("2015-01-01"),
#'    as.Date("2013-01-01"),
#'    as.Date("2013-01-01")
#'  )
#')
#'
#'obs_1 <- tibble::tibble(
#'  observation_period_id = c("1", "2", "3"),
#'  person_id = c("1", "2", "3"),
#'  observation_period_start_date = c(
#'    as.Date("2010-02-03"),
#'    as.Date("2010-02-01"),
#'    as.Date("2010-01-01")
#'  ),
#'  observation_period_end_date = c(
#'    as.Date("2014-01-01"),
#'    as.Date("2012-01-01"),
#'    as.Date("2012-01-01")
#'  )
#')
#'
#'cdm <-
#'  mockCohortProfiles(
#'    seed = 1,
#'    cohort1 = cohort1,
#'    observation_period = obs_1
#'
#'  )
#'
#'result <- cdm$cohort1 %>% addPriorHistory(cdm) %>% dplyr::collect()
#' }

addObservationPeriod <- function(x,
                                 cdm,
                                 observationAt = "cohort_start_date",
                                 column = c("observation_period_start_date",
                                            "observation_period_end_date"),
                                 name = NULL,
                                 tablePrefix = NULL) {
  # if name is NULL replace name with column

  if (is.null(name)) {
    name = column
  } else {
    name = name
  }

  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()



  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push("- x is not a table")
  }

  columnCheck <-
    ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push("- neither `subject_id` nor `person_id` are columns of x")
  }

  column1Check <- observationAt %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push("- `observationAt` is not a column of x")
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


  #checks for name and column
  checkmate::assertCharacter(column, len = 2, add = errorMessage)
  checkmate::assertCharacter(name, len = 2, add = errorMessage)
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # rename so x contain subject_id

  if ("subject_id" %in% colnames(x) == FALSE) {
    x <- x %>%
      dplyr::rename("subject_id" = "person_id")
  } else {
    x <- x
  }

  xOutput <- x %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select("subject_id" = "person_id",
                      dplyr::all_of(column)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(ins = dplyr::if_else(.data[[observationAt]] >= .data[[column[1]]] &
                                         .data[[observationAt]] <= .data[[column[2]]],
                                       1,
                                       0)) %>% dplyr::mutate(
                                         !!column[[1]] := dplyr::if_else(ins == 0, NA, .data[[column[1]]]),!!column[[2]] := dplyr::if_else(ins == 0, NA, .data[[column[2]]])
                                       ) %>%
    dplyr::select(-ins) %>%
    dplyr::rename(!!name[[1]] := .data[[column[1]]],!!name[[2]] := .data[[column[2]]])
  # Warning message if multiple obersvational_period are found
  if ((dplyr::count(xOutput) %>% dplyr::collect()) > (dplyr::count(x) %>% dplyr::collect())) {
    warning(
      "The Output contain more rows than x,multiple observational period found in observational_period table for rows of x"
    )
  }

  if(is.null(tablePrefix)){
    xOutput <- xOutput %>%
      CDMConnector::computeQuery()
  } else {
    xOutput <- xOutput %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }

  return(xOutput)

}



