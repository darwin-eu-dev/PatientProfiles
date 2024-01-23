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

#' Summarise characteristics of individuals
#'
#' @param cohort A cohort in the cdm
#' @param cdm A cdm reference.
#' @param strata Stratification list
#' @param ageGroup A list of age groups.
#' @param tableIntersect A list of arguments that uses addTableIntersect
#' function to add covariates and comorbidities.
#' @param cohortIntersect A list of arguments that uses addCohortIntersect
#' function to add covariates and comorbidities.
#' @param minCellCount minimum counts due to obscure
#'
#' @return A summary of the characteristics of the individuals
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     "Visits" = list(
#'       tableName = "visit_occurrence", value = "count", window = c(-365, 0)
#'     )
#'   ),
#'   cohortIntersect = list(
#'     "Medications" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
#'     )
#'   )
#' )
#' }
summariseCharacteristics <- function(cohort,
                                     cdm = attr(cohort, "cdm_reference"),
                                     strata = list(),
                                     ageGroup = NULL,
                                     tableIntersect = list(
                                       "Visit" = list(
                                         tableName = "visit_occurrence",
                                         value = "count", window = c(-365, 0)
                                       )
                                     ),
                                     cohortIntersect = list(),
                                     minCellCount = 5) {
  # check initial tables
  checkX(cohort)
  checkCdm(cdm)
  checkStrata(strata, cohort)
  checkAgeGroup(ageGroup)
  checkmate::assertIntegerish(minCellCount, lower = 1)
  checkTableIntersect(tableIntersect, cdm)
  checkCohortIntersect(cohortIntersect, cdm)

  # add names
  ageGroup <- checkAgeGroup(ageGroup)
  tableIntersect <- lapply(tableIntersect, function(x) {
    if (!is.list(x[["window"]])) {
      x[["window"]] <- list(x[["window"]])
    }
    names(x[["window"]]) <- getWindowNames(x[["window"]])
    return(x)
  })
  cohortIntersect <- lapply(cohortIntersect, function(x) {
    if (!is.list(x[["window"]])) {
      x[["window"]] <- list(x[["window"]])
    }
    names(x[["window"]]) <- getWindowNames(x[["window"]])
    return(x)
  })

  # add baseline characteristics
  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", dplyr::all_of(unique(unlist(strata)))
    ) %>%
    addDemographics(ageGroup = ageGroup)

  variables <- dplyr::tibble(
    variable = c("cohort_start_date", "cohort_end_date"),
    variable_type = "date"
  ) %>%
    dplyr::union_all(dplyr::tibble(
      variable = c("age", "prior_observation", "future_observation"),
      variable_type = "numeric"
    )) %>%
    dplyr::union_all(dplyr::tibble(
      variable = c("sex", names(ageGroup)),
      variable_type = "categorical"
    ))

  # add tableIntersect
  for (k in seq_along(tableIntersect)) {
    columTableIntersect <- colnames(cohort)
    cohort <- cohort %>%
      PatientProfiles::addIntersect(
        cdm = cdm,
        tableName = tableIntersect[[k]][["tableName"]],
        value = tableIntersect[[k]][["value"]],
        window = tableIntersect[[k]][["window"]],
        nameStyle = paste0(
          tableIntersect[[k]][["tableName"]], "_any_{value}_{window_name}"
        )
      )
    columTableIntersect <- setdiff(colnames(cohort), columTableIntersect)
    variables <- variables %>%
      dplyr::bind_rows(dplyr::tibble(
        variable = columTableIntersect,
        variable_type = switch(tableIntersect[[k]][["value"]],
          "flag" = "binary",
          "date" = "date",
          "time" = "numeric",
          "count" = "numeric",
          "categorical"
        ),
        variable_group = names(tableIntersect)[k]
      ))
  }

  # add cohortIntersect
  for (k in seq_along(cohortIntersect)) {
    columCohortIntersect <- colnames(cohort)
    cohort <- cohort %>%
      PatientProfiles::addCohortIntersectFlag(
        cdm = cdm,
        targetCohortTable = cohortIntersect[[k]][["targetCohortTable"]],
        window = cohortIntersect[[k]][["window"]],
        nameStyle = paste0(
          cohortIntersect[[k]][["targetCohortTable"]],
          "_{cohort_name}_{value}_{window_name}"
        )
      )
    columCohortIntersect <- setdiff(colnames(cohort), columCohortIntersect)
    variables <- variables %>%
      dplyr::bind_rows(dplyr::tibble(
        variable = columCohortIntersect,
        variable_type = switch(cohortIntersect[[k]][["value"]],
          "flag" = "binary",
          "date" = "date",
          "time" = "numeric",
          "count" = "numeric"
        ),
        variable_group = names(cohortIntersect)[k]
      ))
  }

  # set variables
  variablesToSummary <- list()
  keys <- variables %>%
    dplyr::pull("variable_type") %>%
    unique()
  for (k in seq_along(keys)) {
    variablesToSummary[[keys[k]]] <- variables %>%
      dplyr::filter(.data$variable_type == .env$keys[k]) %>%
      dplyr::pull("variable")
  }

  # set functions
  functionsToSummary <- list(
    date = c("median", "min", "q25", "q75", "max"),
    numeric = c("median", "min", "q25", "q75", "max"),
    categorical = c("count", "percentage"),
    binary = c("count", "percentage")
  )
  functionsToSummary <- functionsToSummary[names(variablesToSummary)]

  # update cohort_names
  cohort <- cohort %>% addCohortName()

  # summarise results
  results <- cohort %>%
    summariseResult(
      group = list("cohort_name"), strata = strata,
      variables = variablesToSummary, functions = functionsToSummary,
      minCellCount = minCellCount
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(result_type = "Summary characteristics")


  if (cohort %>%
    dplyr::count() %>%
    dplyr::pull() > 0) {
    # style intersects
    results <- tidyResults(results, variables, c(tableIntersect, cohortIntersect))

    # select variables
    results <- results %>%
      dplyr::select(
        "cdm_name", "result_type", "group_name", "group_level", "strata_name",
        "strata_level", "variable", "variable_level", "variable_type",
        "estimate_type", "estimate"
      )
  }

  return(results)
}

tidyResults <- function(results, variables, intersect) {
  tidyColumn <- function(col) {
    stringr::str_to_sentence(gsub("_", " ", col))
  }
  if (length(intersect) != 0) {
    patternNames <- lapply(names(intersect), function(x) {
      tidyr::expand_grid(
        value = intersect[[x]][["value"]],
        window_name = names(intersect[[x]][["window"]]),
        table_name = c(
          intersect[[x]][["targetCohortTable"]],
          intersect[[x]][["tableName"]]
        )
      ) %>%
        dplyr::mutate(variable_group = .env$x)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::rowwise() %>%
      tidyr::separate(
        col = "window_name", into = "window_first", sep = "_", remove = FALSE,
        extra = "drop"
      ) %>%
      dplyr::mutate(
        pattern = paste0("_", .data$value, "_", .data$window_name),
        variable_new = paste(
          .data$variable_group, .data$value, dplyr::if_else(
            substr(.data$window_first, 1, 1) == "m" &
              suppressWarnings(!is.na(as.numeric(substr(
                .data$window_first, 2, nchar(.data$window_first)
              )))),
            gsub("m", "-", paste(.data$window_name, "days")), .data$window_name
          )
        )
      ) %>%
      dplyr::select("pattern", "table_name", "variable_group", "variable_new")

    results %>%
      dplyr::left_join(
        variables %>%
          dplyr::select(-"variable_type") %>%
          dplyr::filter(!is.na(.data$variable_group)) %>%
          dplyr::inner_join(patternNames, by = "variable_group") %>%
          dplyr::rowwise() %>%
          dplyr::filter(
            grepl(.data$pattern, .data$variable) &
              grepl(.data$table_name, .data$variable)
          ) %>%
          dplyr::mutate(
            variable_level_new = gsub(.data$pattern, "", .data$variable)
          ) %>%
          dplyr::mutate(variable_level_new = gsub(
            paste0(.data$table_name, "_"), "", .data$variable_level_new
          )) %>%
          dplyr::select(-c("pattern", "variable_group", "table_name")),
        by = "variable"
      ) %>%
      dplyr::mutate(
        variable = dplyr::if_else(
          is.na(.data$variable_new), .data$variable,
          .data$variable_new
        ),
        variable_level = dplyr::if_else(
          is.na(.data$variable_level_new), .data$variable_level,
          .data$variable_level_new
        )
      ) %>%
      dplyr::mutate(dplyr::across(
        c("group_level", "strata_level", "variable", "variable_level"),
        tidyColumn
      ))
  } else {
    results %>%
      dplyr::mutate(dplyr::across(
        c("group_level", "strata_level", "variable", "variable_level"),
        tidyColumn
      ))
  }
}
