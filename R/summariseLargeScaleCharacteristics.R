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
#' #cdm$cohort1 %>%
#' #   summariseLargeScaleCharacteristics(
#' #    window = list(c(-180, -1), c(0, 0), c(1, 180)),
#' #     eventInWindow = "condition_occurrence",
#' #    episodeInWindow = "drug_exposure"
#' #   )
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
                                               episodeInWindow = c("drug_exposure", "ATC 1st"),
                                               includeSource = FALSE,
                                               minCellCount = 5,
                                               cdm = attr(cohort, "cdm_reference")) {
  # add names to windows
  names(window) <- gsub("_", " ", gsub("m", "-", getWindowNames(window)))

  writeSchema <- attr(cdm, "write_schema")

  # initial table
  x <- cohort %>%
    addDemographics(
      age = FALSE, sex = FALSE, priorObservationName = "start_obs",
      futureObservationName = "end_obs"
    ) %>%
    dplyr::mutate(start_obs = -.data$start_obs) %>%
    dplyr::select("subject_id", "cohort_start_date", "start_obs", "end_obs") %>%
    dplyr::distinct() %>%
    dbplyr::window_order(.data$subject_id, .data$cohort_start_date) %>%
    dplyr::mutate(obs_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    CDMConnector::computeQuery(
      name = "lsc_individuals", temporary = FALSE, schema = writeSchema,
      overwrite = TRUE
    )

  # get analysis table
  atc <- c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th")
  icd10 <- c("icd10 chapter", "icd10 subchapter")
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
      "start_diff" = getStartName(tab),
      "end_diff" = ifelse(is.na(getEndName(tab)), getStartName(tab), getEndName(tab)),
      "standard" = getConceptName(tab),
      "source" = getSourceConceptName(tab)
    )
    if (includeSource == FALSE) {
      toSelect <- toSelect["source" != names(toSelect)]
    }
    table <- cdm[[tab]] %>%
      dplyr::select(dplyr::all_of(toSelect)) %>%
      dplyr::inner_join(x, by = "subject_id") %>%
      dplyr::mutate(end_diff = dplyr::if_else(
        is.na(.data$end_diff), .data$start_diff, .data$end_diff
      )) %>%
      dplyr::mutate(start_diff = !!CDMConnector::datediff(
        "cohort_start_date", "start_diff"
      )) %>%
      dplyr::mutate(end_diff = !!CDMConnector::datediff(
        "cohort_start_date", "end_diff"
      )) %>%
      dplyr::filter(
        .data$end_diff >= .data$start_obs & .data$start_diff <= .data$end_obs
      )
    if (!is.infinite(minWindow)) {
      table <- table %>%
        dplyr::filter(.data$end_diff >= .env$minWindow)
    }
    if (!is.infinite(maxWindow)) {
      table <- table %>%
        dplyr::filter(.data$start_diff <= .env$maxWindow)
    }
    table <- table %>%
      dplyr::select(-"start_obs", -"end_obs") %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery(
        name = "lsc_table", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    for (k in seq_len(nrow(analysesTable))) {
      type <- analysesTable$type[k]
      analysis <- analysesTable$analysis[k]
      lsc <- lsc %>%
        dplyr::bind_rows(
          getLsc(cohort, table, strata, window, type, analysis, writeSchema, cdm) %>%
            dplyr::mutate(table_name = tab)
        )
      if (includeSource & analysis == "standard" & !is.na(getSourceConceptName(tab))) {
        lsc <- lsc %>%
          dplyr::bind_rows(
            getLsc(cohort, table, strata, window, type, "source", writeSchema, cdm) %>%
              dplyr::mutate(table_name = tab)
          )
      }
    }
  }

  # calculate denominators
  den <- denominatorCounts(cohort, x, strata, window, writeSchema)

  # format results
  results <- lsc %>%
    formatLscResult(den, cdm)

  # eliminate permanent tables
  CDMConnector::dropTable(cdm = cdm, name = c(
    "lsc_individuals", "lsc_table", "lsc_table_window",
    "lsc_table_window_cohort"
  ))

  # suppress counts
  lsc <- suppressCounts(result = lsc, minCellCount = minCellCount)

  # return
  return(lsc)
}

getLsc <- function(cohort, table, strata, window, type, analysis, writeSchema, cdm) {
  if (type == "event") {
    table <- table %>%
      dplyr::mutate("end_diff" = .data$start_diff)
  }
  if (analysis %in% c("standard", "source")) {
    table <- table %>%
      dplyr::rename("concept" = dplyr::all_of(analysis)) %>%
      dplyr::select(-dplyr::any_of(c("standard", "source")))
    result <- getLscConcept(cohort, table, strata, window, writeSchema)
  } else {
    result <- getLscGroup(cohort, table, strata, window, analysis, writeSchema, cdm)
  }
  result <- result %>%
    dplyr::mutate(type = type, analysis = analysis)
  return(result)
}
getLscConcept <- function(cohort, table, strata, window, writeSchema) {
  result <- NULL
  for (k in seq_along(window)) {
    startWindow <- window[[k]][1]
    endWindow <- window[[k]][2]
    if (is.infinite(startWindow)) {
      if (is.infinite(endWindow)) {
        tableWindow <- table
      } else {
        tableWindow <- table %>%
          dplyr::filter(.data$start_diff <= .env$endWindow)
      }
    } else {
      if (is.infinite(endWindow)) {
        tableWindow <- table %>%
          dplyr::filter(.data$end_diff >= .env$startWindow)
      } else {
        tableWindow <- table %>%
          dplyr::filter(
            .data$end_diff >= .env$startWindow &
              .data$start_diff <= .env$endWindow
          )
      }
    }
    tableWindow <- tableWindow %>%
      dplyr::select("subject_id", "cohort_start_date", "obs_id", "concept") %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery(
        name = "lsc_table_window", temporary = FALSE, schema = writeSchema,
        overwrite = TRUE
      )
    result <- result %>%
      dplyr::bind_rows(
        summariseConcept(cohort, tableWindow, strata, writeSchema) %>%
          dplyr::mutate(window_name = names(window)[k])
      )
  }
  return(result)
}
summariseConcept <- function(cohort, tableWindow, strata, writeSchema) {
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
        name = "lsc_table_window_cohort", temporary = FALSE,
        schema = writeSchema, overwrite = TRUE
      )
    result <- result %>%
      dplyr::bind_rows(
        tableWindowCohort %>%
          dplyr::group_by(.data$concept) %>%
          dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
          dplyr::collect() %>%
          dplyr::mutate(strata_name = "Overall", strata_level = "Overall") %>%
          dplyr::bind_rows(summariseStrataCounts(tableWindowCohort, strata)) %>%
          dplyr::mutate(group_name = "Cohort name", group_level = cohortName)
      )
  }
  return(result)
}
summariseStrataCounts <- function(tableWindowCohort, strata) {
  result <- NULL
  for (k in seq_along(strata)) {
    result <- result %>%
      dplyr::union_all(
        tableWindowCohort %>%
          dplyr::select("obs_id", dplyr::all_of(strata[[k]])) %>%
          dplyr::group_by(dplyr::all_of(c("concept", strata[[k]]))) %>%
          dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
          dplyr::collect() %>%
          tidyr::unite(dplyr::all_of(strata[[k]]), sep = " and ") %>%
          dplyr::mutate(strata_name = paste0(strata[[k]], collapse = " and "))
      )
  }
  return(result)
}
denominatorCounts <- function(cohort, x, strata, window, writeSchema) {
  table <- x %>%
    dplyr::rename("start_diff" = "start_obs", "end_diff" = "end_obs") %>%
    dplyr::mutate(concept = "denominator")
  result <- getLscConcept(cohort, table, strata, window, writeSchema)

}
formatLscResult <- function(lsc, den, cdm) {
  lsc %>%
    dplyr::inner_join(
      den %>%
        dplyr::rename("denominator" = "count") %>%
        dplyr::select(-"concept"),
      by = c(
        "strata_name", "strata_level", "group_name", "group_level",
        "window_name"
      )
    ) %>%
    dplyr::mutate(percentage = 100 * .data$count / .data$denominator) %>%
    dplyr::select(-"denominator") %>%
    tidyr::pivot_longer(
      cols = c("count", "percentage"), names_to = "estimate_type",
      values_to = "estimate"
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(
      estimate = as.character(.data$estimate),
      result_type = "Summarised Large Scale Characteristics"
    ) %>%
    dplyr::inner_join(addConceptName(lsc, cdm), by = c("concept", "analysis")) %>%
    dplyr::select(
      "result_type", "cdm_name", "group_name", "group_level", "strata_name",
      "strata_level", "table_name", "type", "analysis", "window_name",
      "concept", "concept_name", "estimate_type", "estimate"
    )
}
addConceptName <- function(lsc, cdm) {
  concepts <- lsc %>%
    dplyr::select("concept", "analysis") %>%
    dplyr::distinct()
  conceptNames <- cdm[["concept"]] %>%
    dplyr::select("concept" = "concept_id", "concept_name") %>%
    dplyr::inner_join(
      concepts %>%
        dplyr::mutate(concept = as.numeric(.data$concept)),
      by = "concept_id",
      copy = TRUE
    ) %>%
    dplyr::collect()
  return(conceptNames)
}
getLscGroup <- function(cohort, table, strata, window, analysis, writeSchema, cdm) {
  if (analysis %in% c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5h")) {
    codes <- cdm[["concept"]] %>%
      dplyr::filter(.data$vocabulary_id == "ATC") %>%
      dplyr::filter(.data$concept_class_id == .env$analysis) %>%
      dplyr::select("concept_new" = "concept_id") %>%
      dplyr::inner_join(
        cdm[["concept_ancestor"]] %>%
          dplyr::select(
            "concept_id" = "ancestor_concept_id",
            "concept_new" = "descendant_concept_id"
          ),
        by = "concept_id"
      )
  } else {
    codes <- cdm[["concept"]] %>%
      dplyr::filter(.data$vocabulary_id == "ICD10") %>%
      dplyr::filter(.data$concept_class_id == .env$analysis) %>%
      dplyr::select("concept_new" = "concept_id")
    ## TO DO ##
  }
  table <- table %>%
    dplyr::inner_join(codes, by = "concept") %>%
    dplyr::select(-"concept") %>%
    dplyr::rename("concept" = "concept_new") %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery(
      name = "lsc_table_group", temporary = FALSE, schema = writeSchema,
      overwrite = TRUE
    )
  result <- getLscConcept(cohort, table, strata, window, writeSchema)
  return(result)
}
