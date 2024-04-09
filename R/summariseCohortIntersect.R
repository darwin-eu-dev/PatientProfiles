# Copyright 2024 DARWIN EU (C)
#
# This file is part of PatientProfiles
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

#' Summarise cohort intersection information
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param cohort A cohort in the cdm.
#' @param cohortIntersect The settings for cohort intersection settings.
#' @param strata Stratification list.
#'
#' @return A summary of the cohort intersection informations.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' summariseCohortIntersect(
#'   cohort = cdm$cohort1,
#'   cohortIntersect = list(
#'     "Medications in the prior year" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'     )
#'   )
#' )
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
summariseCohortIntersect <- function(cohort,
                                     cohortIntersect,
                                     strata = list()){
  lifecycle::deprecate_soft(
    when = "0.8.0",
    what = "PatientProfiles::summariseCohortIntersect()",
    with = "CohortCharacteristics::summariseCohortIntersect()"
  )
  if (length(cohortIntersect) == 0) {
    cli::cli_abort("Please provide at least a cohort to intersect with, see addCohortIntersect arguments and examples for help.")
  }
  summariseCharacteristics(
    cohort = cohort,
    strata = strata,
    ageGroup = NULL,
    demographics = FALSE,
    cohortIntersect = cohortIntersect,
    tableIntersect = list(),
    conceptIntersect = list(),
    otherVariables = character()
  ) |>
    dplyr::filter(.data$result_type == "summarised_cohort_intersect")
}
