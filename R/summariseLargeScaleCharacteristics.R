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
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
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
                                               incidentAtc = NULL,
                                               overlapAtc = NULL,
                                               incidentIcd10 = NULL,
                                               overlapIcd10 = NULL,
                                               minCellCount = 5,
                                               cdm = attr(cohort, "cdm_reference")) {
  # edit write prefix
  writeSchema <- attr(attr(cdm, "cdm_reference"), "write_schema")
  if ("prefix" %in% names(writeSchema)) {
    writeSchema["prefix"] <- paste0(writeSchema["prefix"], "lsc_")
  } else {
    writeSchema["prefix"] <- "lsc_"
  }

  # initial table
  x <- cohort %>%
    dplyr::select("subejct_id", "cohort_start_date") %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(obs_id = dplyr::row_number()) %>%
    CDMConnector::computeQuery(
      name = "individuals", temporary = FALSE, schema = writeSchema,
      overwrite = TRUE
    )

  # get analysis table
  analysis <- list(
    dplyr::tibble(
      table = incidentStandard, event = "incident", concept = "standard",
      name = "standard"
    ),
    dplyr::tibble(
      table = overlapStandard, event = "overlap", concept = "standard",
      name = "standard"
    ),
    dplyr::tibble(
      table = incidentSource, event = "incident", concept = "source",
      name = "source"
    ),
    dplyr::tibble(
      table = overlapSource, event = "overlap", concept = "source",
      name = "source"
    ),
    dplyr::tibble(
      table = "drug_exposure", event = "incident", concept = "standard",
      name = incidentAtc
    ),
    dplyr::tibble(
      table = "drug_exposure", event = "overlap", concept = "standard",
      name = overlapAtc
    ),
    dplyr::tibble(
      table = "condition_occurrence", event = "incident", concept = "standard",
      name = incidentIcd10
    ),
    dplyr::tibble(
      table = "condition_occurrence", event = "overlap", concept = "standard",
      name = overlapIcd10
    )
  ) %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na()

  # perform lsc
  lsc <- NULL
  for (tab in unique(analysis$table)) {
    analysisTable <- analysis %>% dplyr::filter(.data$table == .env$tab)
    toSelect <- c(
      "subject_id" = "person_id",
      "start" = getStartName(tab),
      ifelse(is.na(getEndName(tab)), NULL, "end" = getEndName(tab)),
      ifelse(
        "standard" %in% analysisTable$concept,
        "standard" = getConceptName(tab), NULL
      ),
      ifelse(
        "source" %in% analysisTable$concept,
        "source" = getSourceConceptName(tab), NULL
      )
    )
    table <- cdm[[tab]] %>%
      dplyr::select(dplyr::all_of(toSelect)) %>%
      dplyr::inner_join(x, by = "subject_id") %>%
      dplyr::mutate(
        start = !!CDMConnector::datediff("cohort_start_date", "start")
      )
    if ("end" %in% names(toSelect)) {
      table <- table %>%
        dplyr::mutate(
          end = !!CDMConnector::datediff("cohort_start_date", "end")
        )
    }
    table <- table %>%
      CDMConnector::computeQuery(
        name = "table", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    for (k in seq_len(nrow(analysisTable))) {
      lsc <- lsc %>%
        dplyr::union_all(
          getLsc(x, table, tab, strata, window, analysisTable[k, ])
        )
    }
  }

  # suppress counts
  lsc <- suppressCounts(lsc)

  # return
  return(lsc)
}

