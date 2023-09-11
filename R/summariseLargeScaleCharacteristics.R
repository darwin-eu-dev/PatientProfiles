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
#' @param eventInWindow Tables to characterise the events in the window.
#' @param episodeInWindow Tables to characterise the episodes in the window.
#' @param includeSource Whether to include source concepts.
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
                                               eventInWindow = "condition_occurrence",
                                               episodeInWindow = "drug_exposure",
                                               includeSource = FALSE,
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
    dbplyr::window_order(.data$subject_id, .data$cohort_start_date) %>%
    dplyr::mutate(obs_id = dplyr::row_number()) %>%
    dbply::window_order() %>%
    CDMConnector::computeQuery(
      name = "individuals", temporary = FALSE, schema = writeSchema,
      overwrite = TRUE
    )

  atc <- c("atc 1st", "atc 2nd", "atc 3rd", "atc 4th", "atc 5th")
  icd10 <- c("icd10 chapter", "icd10 subchapter")
  # get analysis table
  analyses <- list(
    dplyr::tibble(
      table = eventInWindow[!(eventInWindow %in% c(atc, icd10))],
      type = "event", analysis = "standard"
    ),
    dplyr::tibble(
      table = episodeInWindow[!(episodeInWindow %in% c(atc, icd10))],
      type = "episode", analysis = "standard"
    ),
    dplyr::tibble(
      table = "drug_exposure", type = "event",
      analysis = eventInWindow[eventInWindow %in% atc],
    ),
    dplyr::tibble(
      table = "drug_exposure", type = "episode",
      analysis = episodeInWindow[episodeInWindow %in% atc],
    ),
    dplyr::tibble(
      table = "condition_occurrence", type = "event",
      analysis = eventInWindow[eventInWindow %in% icd10],
    ),
    dplyr::tibble(
      table = "condition_occurrence", type = "episode",
      analysis = episodeInWindow[episodeInWindow %in% icd10],
    )
  ) %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na()

  minWindow <- min(unlist(window))
  maxWindow <- max(unlist(window))

  # perform lsc
  lsc <- NULL
  for (tab in unique(analyses$table)) {
    analysesTable <- analyses %>% dplyr::filter(.data$table == .env$tab)
    toSelect <- c(
      "subject_id" = "person_id",
      "start" = getStartName(tab),
      ifelse(
        is.na(getEndName(tab)),
        "end" = getStartName(tab),
        "end" = getEndName(tab)
      ),
      "standard" = getConceptName(tab),
      ifelse(includeSource, "source" = getSourceConceptName(tab), NULL)
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
    if (!is.infinite(minWindow)) {
      table <- table %>%
        dplyr::filter(.data$end >= .env$minWindow)
    }
    if (!is.infinite(maxWindow)) {
      table <- table %>%
        dplyr::filter(.data$start <= .env$maxWindow)
    }
    table <- table %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery(
        name = "table", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    for (k in seq_len(nrow(analysesTable))) {
      type <- analysisTable$type[k]
      analysis <- analysesTable$analysis[k]
      lsc <- lsc %>%
        dplyr::union_all(
          getLsc(cohort, table, strata, window, type, analysis) %>%
            dplyr::mutate(table_name = tab)
        )
      if (includeSource & analysis == "standard") {
        lsc <- lsc %>%
          dplyr::union_all(
            getLsc(cohort, table, strata, window, type, "source") %>%
              dplyr::mutate(table_name = tab)
          )
      }
    }
  }

  # suppress counts
  lsc <- suppressCounts(result = lsc, minCellCount = minCellCount)

  # return
  return(lsc)
}

getLsc <- function(cohort, table, strata, window, type, analysis) {
  if (type == "event") {
    table <- table %>%
      dplyr::mutate("end" = .data$start)
  }
  if (analysis %in% c("standard", "source")) {
    table <- table %>%
      dplyr::rename("concept" = dplyr::all_of(analysis)) %>%
      dplyr::select(-dplyr::any_of(c("standard", "source")))
    result <- getLscConcept(cohort, table, strata, window)
  } else {
    result <- getLscGroup(cohort, table, strata, window, analysis)
  }
  result <- result %>%
    dplyr::mutate(type = type, analysis = analysis)
  return(result)
}
getLscConcept <- function(cohort, table, strata, window) {
  result <- NULL
  for (k in seq_along(window)) {
    startWindow <- window[[k]][1]
    endWindow <- window[[k]][2]
    tableWindow <- table %>%
      dplyr::filter(
        .data$end <= .env$startWindow & .data$start >= .env$endWindow
      ) %>%
      dplyr::select("subject_id", "cohort_start_date", "obs_id", "concept") %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery(
        name = "table_window", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    result <- result %>%
      dplyr::union_all(
        summariseConcept(cohort, tableWindow, strata) %>%
          dplyr::mutate(window_name = names(window)[k])
      )
  }
}
summariseConcept <- function(cohort, tableWindow, strata) {
  result <- NULL
  cohortNames <- CDMConnector::cohortSet(cohort)$cohort_name
  for (cohortName in cohortNames) {
    cdi <- CDMConnector::cohortSet(cohort) %>%
      dplyr::filter(.data$cohort_name == .env$cohortName) %>%
      dplyr::pull("cohort_definition_id")
    tableWindowCohort <- tableWindow %>%
      dplyr::inner_join(
        cohort %>%
          dplyr::filter(.data$cohort_definition_id == .env$cdi),
        by = c("subject_id", "cohort_start_date")
      ) %>%
      dplyr::select(
        "obs_id", "concept", dplyr::all_of(unique(unlist(strata)))
      ) %>%
      CDMConnector::computeQuery(
        name = "table_window_cohort", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    result <- result %>%
      dplyr::union_all(
        tableWindowCohort %>%
          dplyr::group_by(.data$concept) %>%
          dplyr::summarise(estimate = dplyr::n(), .groups = "drop") %>%
          dplyr::collect() %>%
          dplyr::mutate(strata_name = "Overall", strata_level = "Overall") %>%
          dplyr::union_all(summariseStrataCounts(tableWindowCohort, strata)) %>%
          dplyr::mutat(group_name = "Cohort name", group_level = cohortName)
      )
  }
}
summariseStrataCounts <- function(tableWindowCohort, strata) {
  result <- NULL
  for (k in seq_along(strata)) {
    tableWindowCohort %>%
      dplyr::select("obs_id", dplyr::all_of(strata[[k]])) %>%
      dplyr::group_by(dplyr::all_of(c("concept", strata[[k]]))) %>%
      dplyr::summarise(estimate = dplyr::n(), .groups = "drop") %>%
      dplyr::collect() %>%
      dplyr::mutate(
        strata_name = paste0(strata[[k]], collapse = " and "),
        strata_level = paste0(collapase())
      )
  }
}
