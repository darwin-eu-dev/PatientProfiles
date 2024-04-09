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

#' Summarise table intersection information
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param cohort A cohort in the cdm.
#' @param tableIntersect A list of arguments that uses addTableIntersect
#' function to add variables to summarise.
#' @param strata Stratification list.
#'
#' @return A summary of the table intersections.

summariseTableIntersect <- function(cohort,
                                    tableIntersect = list(),
                                    strata = list()){
  lifecycle::deprecate_soft(
    when = "0.8.0",
    what = "PatientProfiles::summariseTableIntersect()",
    with = "CohortCharacteristics::summariseTableIntersect()"
  )
  summariseCharacteristics(
    cohort = cohort,
    strata = strata,
    ageGroup = NULL,
    demographics = FALSE,
    cohortIntersect = list(),
    tableIntersect = tableIntersect,
    conceptIntersect = list(),
    otherVariables = character()
  ) |>
    dplyr::filter(.data$result_type == "summarised_demographics")
}
