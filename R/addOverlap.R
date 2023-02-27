# This file is part of CohortProfiles

#' It creates a column to indicate overlaps with the overlapcohort
#'
#' @param x table containing the individual for which the overlap indicator to be attached as extra columns
#' @param cdm cdm containing the tables
#' @param overlapCohortName name of the cohort that we want to check if
#' overlaps with the target cohort
#' @param filter condition to filer the overlapCohortIdName table.
#' @param window window to consider events of, from date of reference in table x
#' to date of event at event table
#' @param name name of the new column created for the overlap indicator
#' @param compute if TRUE compute the output
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#' \dontrun{
#'  cohort1 <- tibble::tibble(
#' cohort_definition_id = c(1, 1, 1, 1),
#' subject_id = c(1, 1, 2, 3),
#' cohort_start_date = c(
#'   as.Date("2010-01-01"), as.Date("2013-01-01"),
#'   as.Date("2010-01-02"), as.Date("2010-01-01")),
#' cohort_end_date = c(
#'   as.Date("2012-01-01"), as.Date("2015-01-01"),
#'   as.Date("2015-01-01"), as.Date("2015-01-01"))
#' )
#'
#' cohort2 <- tibble::tibble(
#'   cohort_definition_id = c(2, 2, 2, 3),
#'   subject_id = c(1, 2, 2, 2),
#'   cohort_start_date = c(
#'    as.Date("2009-12-01"), as.Date("2009-01-01"),
#'     as.Date("2009-11-01"), as.Date("2009-11-01")),
#'   cohort_end_date = c(
#'     as.Date("2011-01-01"), as.Date("2009-01-02"),
#'     as.Date("2015-01-01"), as.Date("2015-01-01"))
#' )
#'
#' cdm <- mockCohortProfiles(cohort1 = cohort1, cohort2 = cohort2)
#'
#' result_1 <- addOverlap(
#'   cdm$cohort1,
#'   cdm = cdm,
#'   overlapCohortName = "cohort2",
#'   filter = NULL,
#'   window = c(0, 0),
#'   name = "overlap",
#'   compute = TRUE
#' )
#'
#' }
addOverlap <- function(x,
                       cdm,
                       overlapCohortName,
                       filter = NULL,
                       window = c(NA, NA),
                       name = "overlap",
                       compute = TRUE) {
  ## check for user inputs
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push("- x is not a table")
  }
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push("- cdm must be a CDMConnector CDM reference object")
  }
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  #check if overlapCohortName in cdm
  column2Check <- overlapCohortName %in% names(cdm)
  if (!isTRUE(column2Check)) {
    errorMessage$push("- `overlapCohortName` is not found in cdm")
  }
  checkmate::reportAssertions(collection = errorMessage)


  #check if filers covariates in overlapcohortname
  if (!is.null(filter)) {
    checkmate::assertCharacter(names(filter),
                               add = errorMessage,)
    columnsfilterCheck <-
      names(filter) %in% colnames(cdm[[overlapCohortName]])
    if (!isTRUE(columnsfilterCheck)) {
      errorMessage$push("- `the variables in filter` are not found in cdm[[overlapCohortName]]")
    }
  }

  checkmate::assert_integerish(window, len = 2, null.ok = TRUE)

  errorMessage <- checkmate::makeAssertCollection()


  # define overlapcohort table from cdm containing the events of interests
  overlapCohort <- cdm[[overlapCohortName]]
  # define event of interests from filter

  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      overlapCohort <- overlapCohort %>%
        dplyr::filter_at(dplyr::vars(dplyr::all_of(namesFilter[k])),
                         dplyr::any_vars(. %in% !!filter[[k]]))
    }
  }

  #generate overlappingcohort using code from getoverlappingcohort
  overlapCohort <- overlapCohort %>%
    dplyr::rename(
      "overlap_start_date" = "cohort_start_date",
      "overlap_end_date" = "cohort_end_date",
      "overlap_id" = "cohort_definition_id"
    )

  result <- x %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlapCohort, by = "subject_id")
  if (!is.na(window[2])) {
    result <- result %>%
      dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
        CDMConnector::dateadd(date = "overlap_start_date",
                              number = !!-window[2])
      ))) %>%
      dplyr::filter(.data$cohort_start_date >= .data$overlap_start_date)
  }
  if (!is.na(window[1])) {
    result <- result %>%
      dplyr::mutate(overlap_end_date = as.Date(dbplyr::sql(
        CDMConnector::dateadd(date = "overlap_end_date",
                              number = !!-window[1])
      ))) %>%
      dplyr::filter(.data$cohort_start_date <= .data$overlap_end_date)
  }
  result <- result %>%
    dplyr::select("subject_id",
                  "cohort_start_date",
                  "cohort_end_date",
                  "overlap_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(indicator = 1) %>%
    dplyr::mutate(
      overlap_id = as.numeric(.data$overlap_id),
      overlapCohortTableName = .env$overlapCohortName
    ) %>%
    tidyr::pivot_wider(
      names_from = c("overlapCohortTableName", "overlap_id"),
      values_from = "indicator",
      names_glue = "overlap_{overlapCohortTableName}_{overlap_id}",
      values_fill = 0
    ) %>%
    dplyr::right_join(x,
                      by = c("subject_id", "cohort_start_date", "cohort_end_date")) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("overlap"),
      ~ dplyr::if_else(is.na(.x), 0, .x)
    ))
  #column to keep
  dropCols <-
    result %>% dplyr::select(dplyr::starts_with("overlap_")) %>% colnames()

  #generate true/false for name column
  result <- result %>%
    dplyr::mutate(!!name := +(as.numeric(dplyr::if_any(
      dplyr::starts_with("overlap_"),  ~ . == 1
    )))) %>% dplyr::select(-dplyr::all_of(dropCols))

  if (isTRUE(compute)) {
    result <- result %>% dplyr::compute()
  }
  return(result)
}
