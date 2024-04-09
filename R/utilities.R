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

#' Add cohort name for each cohort_definition_id
#'
#' @param cohort cohort to which add the cohort name
#'
#' @return cohort with an extra column with the cohort names
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addCohortName()
#' }
#'
addCohortName <- function(cohort) {
  cohort %>%
    dplyr::left_join(
      attr(cohort, "cohort_set") %>%
        dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_definition_id"
    )
}

#' Add cdm name
#'
#' @param table Table in the cdm
#' @param cdm A cdm reference object
#'
#' @return Table with an extra column with the cdm names
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addCdmName()
#' }
#'
addCdmName <- function(table, cdm = omopgenerics::cdmReference(table)) {
  if (is.null(cdm)) {
    name <- NA_character_
  } else {
    name <- omopgenerics::cdmName(cdm)
  }
  table %>% dplyr::mutate("cdm_name" = .env$name)
}
