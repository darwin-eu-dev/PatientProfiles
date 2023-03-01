# Copyright 2022 DARWIN EU®
#
# This file is part of CohortProfiles
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' This subject if in a certain window the target cohort overlaps with the
#' overlap cohort
#'
#' @param cdm CDMConnector CDM reference object
#' @param targetCohortName name of the cohort that we want to add the
#' overlapping
#' @param targetCohortId id or vector of ids, of the target cohort
#' @param overlapCohortName name of the cohort that we want to check if
#' overlaps with the target cohort
#' @param overlapCohortId id or vector of ids, of the cohort we are interested
#' in
#' @param lookbackWindow lookback period window in days, a list consist of two
#' numbers
#' @param multipleEvents if we want to count the number of evenets (TRUE) then
#' the result is an integer orjust their presence (FALSE) then the result is 1
#' or 0.
#'
#' @return it add a new column to target cohort for each id in overlap cohort.
#' The column is numeric and can be 0 if no overlap is observed and 1 if overlap
#' is observed.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' cohort <- tibble::tibble(
#'   cohort_definition_id = c("1", "2", "1", "2", "2"),
#'   subject_id = c("1", "1", "2", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-01-01"), as.Date("2009-12-01"),
#'     as.Date("2010-01-01"), as.Date("2009-01-01"),
#'     as.Date("2010-01-01")
#'   ), cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"),
#'     as.Date("2015-01-01"), as.Date("2009-01-02"),
#'     as.Date("2015-01-01")
#'   )
#' )
#' cdm <- mockDrugUtilisation(
#'   cohort = cohort
#' )
#' getOverlappingCohort(
#'   cdm = cdm,
#'   targetCohortName = "cohort",
#'   targetCohortId = 1,
#'   overlapCohortName = "cohort",
#'   overlapCohortId = c(2, 3),
#'   lookbackWindow = c(-180, 0)
#' )
#' }
#'
getOverlappingCohort <- function(cdm,
                                 targetCohortName,
                                 targetCohortId = NULL,
                                 overlapCohortName,
                                 overlapCohortId = NULL,
                                 lookbackWindow = 0,
                                 multipleEvents = FALSE) {
  if (is.character(targetCohortId)) {
    targetCohortId <- as.numeric(targetCohortId)
  }
  if (is.character(overlapCohortId)) {
    overlapCohortId <- as.numeric(overlapCohortId)
  }
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(targetCohortName %in% names(cdm), add = errorMessage)
  checkmate::assertIntegerish(
    targetCohortId,
    null.ok = TRUE, add = errorMessage
  )
  checkmate::assertTRUE(overlapCohortName %in% names(cdm), add = errorMessage)
  checkmate::assertIntegerish(
    overlapCohortId,
    null.ok = TRUE, add = errorMessage
  )
  checkmate::assertNumeric(
    lookbackWindow,
    min.len = 1, max.len = 2, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (length(lookbackWindow) == 1) {
    lookbackWindow <- c(lookbackWindow, lookbackWindow)
  }

  if (sum(is.na(lookbackWindow)) == 0) {
    checkmate::assertTRUE(lookbackWindow[1] <= lookbackWindow[2])
  }

  if (is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortName]]
  } else {
    targetCohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  }
  if (is.null(overlapCohortId)) {
    overlapCohort <- cdm[[overlapCohortName]]
  } else {
    overlapCohort <- cdm[[overlapCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$overlapCohortId)
  }

  overlapCohort <- overlapCohort %>%
    dplyr::rename(
      "overlap_start_date" = "cohort_start_date",
      "overlap_end_date" = "cohort_end_date",
      "overlap_id" = "cohort_definition_id"
    )

  result <- targetCohort %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlapCohort, by = "subject_id")
  if (!is.na(lookbackWindow[2])) {
    result <- result %>%
      dplyr::mutate(
        overlap_start_date = as.Date(dbplyr::sql(CDMConnector::dateadd(
          date = "overlap_start_date",
          number = !!-lookbackWindow[2]
        )))
      ) %>%
      dplyr::filter(.data$cohort_start_date >= .data$overlap_start_date)
  }
  if (!is.na(lookbackWindow[1])) {
    result <- result %>%
      dplyr::mutate(
        overlap_end_date = as.Date(dbplyr::sql(CDMConnector::dateadd(
          date = "overlap_end_date",
          number = !!-lookbackWindow[1]
        )))
      ) %>%
      dplyr::filter(.data$cohort_start_date <= .data$overlap_end_date)
  }
  result <- result %>%
    dplyr::select(
      "subject_id", "cohort_start_date", "cohort_end_date", "overlap_id"
    )
  if (multipleEvents == FALSE) {
    result <- result %>%
      dplyr::distinct() %>%
      dplyr::mutate(indicator = 1)
  } else {
    result <- result %>%
      dplyr::group_by(
        .data$subject_id, .data$cohort_start_date, .data$cohort_end_date,
        .data$overlap_id
      ) %>%
      dplyr::summarise(indicator = dplyr::n()) %>%
      dplyr::ungroup()
  }
  result <- result %>%
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
    dplyr::right_join(
      targetCohort,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("overlap"), ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::compute()

  return(result)
}
