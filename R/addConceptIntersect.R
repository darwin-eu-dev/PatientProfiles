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

#' It creates columns to indicate overlap information between a table and a
#' concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence).
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence).
#' @param order last or first date to use for date/time calculations.
#' @param order which record is considered in case of multiple records.
#' @param flag TRUE or FALSE. If TRUE, flag will calculated for this
#' intersection.
#' @param count TRUE or FALSE. If TRUE, the number of counts will be calculated
#' for this intersection.
#' @param date TRUE or FALSE. If TRUE, date will be calculated for this
#' intersection.
#' @param days TRUE or FALSE. If TRUE, time difference in days will be
#' calculated for this intersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' # result <- cdm$cohort1 %>%
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
                                targetEndDate = "cohort_end_date",
                                order = "first",
                                flag = TRUE,
                                count = TRUE,
                                date = TRUE,
                                days = TRUE,
                                nameStyle = "{value}_{concept_name}_{window_name}") {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "addConceptIntersect()",
    details = c(
      "please use the specific functions instead:",
      "*" = "addConceptIntersectFlag()", "*" = "addConceptIntersectCount()",
      "*" = "addConceptIntersectDate()", "*" = "addConceptIntersectDays()"
    )
  )
  .addConceptIntersect(
    x = x, conceptSet = conceptSet, indexDate = indexDate,
    censorDate = censorDate, window = window, targetStartDate = targetStartDate,
    targetEndDate = targetEndDate, order = order, flag = flag, count = count,
    date = date, days = days, nameStyle = nameStyle
  )
}

.addConceptIntersect <- function(x,
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
  cdm <- omopgenerics::cdmReference(x)
  nameCohort <- paste0("concept_cohort_", sample(letters, 5) |> paste0(collapse = ""))
  tempCohort <- paste0("temp_cohort_", sample(letters, 5) |> paste0(collapse = ""))
  personVariable <- colnames(x)[colnames(x) %in% c("person_id", "subject_id")]
  cdm[[tempCohort]] <- cdm[["observation_period"]] |>
    dplyr::inner_join(
      x |>
        dplyr::select("person_id" = dplyr::all_of(personVariable)) |>
        dplyr::distinct(),
      by = "person_id"
    ) |>
    dplyr::mutate("cohort_definition_id" = 1) |>
    dplyr::select(
      "cohort_definition_id", "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) |>
    dplyr::compute(temporary = FALSE, name = tempCohort) |>
    omopgenerics::newCohortTable()
  if (!is.null(targetEndDate) && targetEndDate == "cohort_end_date") {
    end <- "event_end_date"
  } else {
    end <- 0
  }
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = conceptSet,
    subsetCohort = tempCohort,
    name = nameCohort,
    limit = "all",
    end = end,
    overwrite = TRUE
  )
  attr(x, "cdm_reference") <- cdm
  x <- .addCohortIntersect(
    x = x,
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
    nameStyle = gsub("\\{concept_name\\}", "\\{cohort_name\\}", nameStyle)
  )
  toDrop <- c(
    paste0(nameCohort, c("", "_set", "_attrition")),
    paste0(tempCohort, c("", "_set", "_attrition"))
  )
  cdm <- omopgenerics::dropTable(cdm = cdm, name = dplyr::any_of(toDrop))
  return(x)
}

#' It creates column to indicate the flag overlap information between a table
#' and a concept
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
#' # result <- cdm$cohort1 %>%
#' #   addConceptIntersectFlag(
#' #     conceptSet = getDrugIngredientCodes(cdm, "acetaminophen")
#' #  ) %>%
#' #   dplyr::collect()
#' }
#'
addConceptIntersectFlag <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetStartDate = "cohort_start_date",
                                    targetEndDate = NULL,
                                    order = "first",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = order,
    flag = TRUE,
    count = FALSE,
    date = FALSE,
    days = FALSE,
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the count overlap information between a table
#' and a concept
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
#' # result <- cdm$cohort1 %>%
#' #   addConceptIntersectCount(
#' #     conceptSet = getDrugIngredientCodes(cdm, "acetaminophen")
#' #  ) %>%
#' #   dplyr::collect()
#' }
#'
addConceptIntersectCount <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetStartDate = "cohort_start_date",
                                    targetEndDate = NULL,
                                    order = "first",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = order,
    flag = FALSE,
    count = TRUE,
    date = FALSE,
    days = FALSE,
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the date overlap information between a table
#' and a concept
#'
#' @param x Table with individuals in the cdm
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetDate date of reference in cohort table
#' @param order last or first date to use for date/time calculations.
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
#' # result <- cdm$cohort1 %>%
#' #   addConceptIntersectDate(
#' #     conceptSet = getDrugIngredientCodes(cdm, "acetaminophen")
#' #  ) %>%
#' #   dplyr::collect()
#' }
#'
addConceptIntersectDate <- function(x,
                                     conceptSet,
                                     indexDate = "cohort_start_date",
                                     censorDate = NULL,
                                     window = list(c(0, Inf)),
                                     targetDate = "cohort_start_date",
                                     order = "first",
                                     nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetDate,
    targetEndDate = NULL,
    order = order,
    flag = FALSE,
    count = FALSE,
    date = TRUE,
    days = FALSE,
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the days of difference from an index date to a
#' concept
#'
#' @param x Table with individuals in the cdm
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetDate date of reference in cohort table
#' @param order last or first date to use for date/time calculations.
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
#' # result <- cdm$cohort1 %>%
#' #   addConceptIntersectDays(
#' #     conceptSet = getDrugIngredientCodes(cdm, "acetaminophen")
#' #  ) %>%
#' #   dplyr::collect()
#' }
#'
addConceptIntersectDays <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetDate = "cohort_start_date",
                                    order = "first",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetDate,
    targetEndDate = NULL,
    order = order,
    flag = FALSE,
    count = FALSE,
    date = FALSE,
    days = TRUE,
    nameStyle = nameStyle
  )
}
