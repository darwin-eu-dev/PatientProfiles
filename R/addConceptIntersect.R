# Copyright 2023 DARWIN EU (C)
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

#' It creates columns to indicate overlap information between two tables
#'
#' @param x Table with individuals in the cdm
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param order last or first date to use for date/time calculations.
#' @param order which record is considered in case of multiple records
#' @param flag TRUE or FALSE. If TRUE, flag will calculated for this
#' intersection
#' @param count TRUE or FALSE. If TRUE, the number of counts will be calculated
#' for this intersection
#' @param date TRUE or FALSE. If TRUE, date will be calculated for this
#' intersection
#' @param days TRUE or FALSE. If TRUE, time difference in days will be
#' calculated for this intersection
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(CodelistGenerator)
#'
#' cdm <- mockPatientProfiles()
#' #result <- cdm$cohort1 %>%
#' #   addConceptIntersect(
#' #     conceptSet = getDrugIngredientCodes(cdm, "acetaminophen")
#' #  ) %>%
#' #   dplyr::collect()
#' }
#'

addConceptIntersect <- function(x,
                                conceptSet,
                                indexDate = "cohort_start_date",
                                censorDate = NULL,
                                window = list(c(0, Inf)),
                                targetStartDate = "cohort_start_date",
                                targetEndDate = NULL,
                                order = "first",
                                flag = TRUE,
                                count = TRUE,
                                date = TRUE,
                                days = TRUE,
                                nameStyle = "{value}_{concept_name}_{window_name}") {
  cdm = attr(x, "cdm_reference")
  nameCohort <- "add_intersect_concept_set"
  individuals <- x %>%
    dplyr::select(dplyr::any_of(c("subject_id", "person_id"))) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  if (targetEndDate == "cohort_end_date") {
    end <- "event_end_date"
  } else {
    end <- 0
  }
  end <- targetEndDate %||% 0
  cdmNew <- CDMConnector::generateConceptCohortSet(
    cdm = CDMConnector::cdmSubset(cdm, individuals),
    conceptSet = conceptSet,
    name = nameCohort,
    limit = "all",
    end = end
  )
  cdm[[nameCohort]] <- cdmNew[[nameCohort]]
  x <- addCohortIntersect(
    x = x,
    cdm = cdm,
    targetCohortTable = nameCohort,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = order,
    flag = flag,
    count = count,
    date = date,
    days = days,
    nameStyle = gsub("{concept_name}", "{cohort_name}", nameStyle)
  )
  return(x)
}
