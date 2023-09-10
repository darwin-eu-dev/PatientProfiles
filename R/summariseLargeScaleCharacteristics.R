# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
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

#' This function is used to summarise the large scale characteristics of a
#' cohort table
#'
#' @param cohort The cohort to characterise.
#' @param strata Stratification list.
#' @param window Temporal windows that we want to characterize.
#' @param incidentStandard Tables to characterise the concept_id.
#' @param overlapStandard Tables to characterise the concept_id.
#' @param incidentSource Tables to characterise the source_concept_id.
#' @param overlapSource Tables to characterise the source_concept_id.
#' @param incidentAtc ATC levels to characterise.
#' @param overlapAtc ATC levels to characterise.
#' @param incidentIcd10 ICD10 levels to characterise.
#' @param overlapIcd10 ICD10 levels to characterise.
#' @param incidentConceptSet Concept sets to characterise.
#' @param overlapConceptSet Concept sets to characterise.
#' @param minCellCount All counts lower than minCellCount will be obscured.
#' @param cdm A cdm reference.
#'
#' @return The output of this function is a `ResultSummary` containing the
#' relevant information.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm$cohort1 %>%
#'   summariseLargeScaleCharacteristics(
#'     window = list(c(-180, -1), c(0, 0), c(1, 180)),
#'     incidentStandard = "condition_occurrence",
#'     overlapStandard = "drug_expoure"
#'   )
#' }
#'
summariseLargeScaleCharacteristics <- function(cohort,
                                               strata = list(),
                                               window = list(
                                                 c(-Inf, -366), c(-365, -31),
                                                 c(-30, -1), c(0, 0), c(1, 30),
                                                 c(31, 365), c(366, Inf)
                                               ),
                                               incidentStandard = NULL,
                                               overlapStandard = NULL,
                                               incidentSource = NULL,
                                               overlapSource = NULL,
                                               concepSetList = NULL,
                                               incidentAtc = NULL,
                                               overlapAtc = NULL,
                                               incidentIcd10 = NULL,
                                               overlapIcd10 = NULL,
                                               incidentConceptSet = NULL,
                                               overlapConceptSet = NULL,
                                               minCellCount = 5,
                                               cdm = attr(cohort, "cdm_reference")) {
}
