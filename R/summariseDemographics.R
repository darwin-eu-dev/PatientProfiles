# Copyright 2022 DARWIN EU (C)
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

#' Summarise demographics of individuals
#'
#' @param cohort A cohort in the cdm.
#' @param strata Stratification list.
#' @param ageGroup A list of age groups.
#'
#' @return A summary of the demographics of the individuals.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' summariseDemographics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
#' )
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }

summariseDemographics <- function(cohort,
                                  strata = list(),
                                  ageGroup = NULL) {

  results <- PatientProfiles::summariseCharacteristics(
    cohort = cohort,
    strata = strata,
    demographics = TRUE,
    ageGroup = ageGroup
  )
  return(results)
}
