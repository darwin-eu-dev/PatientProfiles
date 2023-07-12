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

#' This function is used to summarise the dose and/or indication over multiple
#' cohorts.
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
#'      )
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
                                       "Visits" = list(
                                         tableName = "visit_occurrence",
                                         value = "count", window = c(-365, 0)
                                       )
                                     ),
                                     cohortIntersect = list(),
                                     minCellCount = 5) {
  # check initial tables
  # checkInputs(
  #   cohort = cohort, cdm = cdm, strata = strata, ageGroup = ageGroup,
  #   windowVisitOcurrence = windowVisitOcurrence, covariates = covariates,
  #   minCellCount = minCellCount
  # )

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
    PatientProfiles::addDemographics(cdm, ageGroup = ageGroup)

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
          tableIntersect[[k]][["tableName"]], "_{value}_{window_name}"
        )
      )
    columTableIntersect <- setdiff(colnames(cohort), columTableIntersect)
    variables <- variables %>%
      dplyr::bind_rows(dplyr::tibble(
        variable = columTableIntersect,
        variable_type = switch(
          tableIntersect[[k]][["value"]], "flag" = "binary", "date" = "date",
          "time" = "numeric", "count" = "numeric", "categorical"
        ),
        group_name = names(tableIntersect)[k]
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
        nameStyle = "{cohort_name}_{value}_{window_name}"
      )
    columCohortIntersect <- setdiff(colnames(cohort), columCohortIntersect)
    variables <- variables %>%
      dplyr::bind_rows(dplyr::tibble(
        variable = columCohortIntersect,
        variable_type = switch(
          cohortIntersect[[k]][["value"]], "flag" = "binary", "date" = "date",
          "time" = "numeric", "count" = "numeric"
        ),
        group_name = names(cohortIntersect)[k]
      ))
  }

  variablesToSummary <- list()
  keys <- variables %>% dplyr::pull("variable_type") %>% unique()
  for (k in seq_along(keys)) {
    variablesToSummary[[keys[k]]] <- variables %>%
      dplyr::filter(.data$variable_type == .env$keys[k]) %>%
      dplyr::pull("variable")
  }

  # set functions
  functionsToSummary <- list(
    date = c("median", "q25", "q75"),
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "%"),
    binary = c("count", "%")
  )
  functionsToSummary <- functionsToSummary[names(variablesToSummary)]

  # update cohort_names
  cohort <- cohort %>% addCohortName()

  # summarise results
  results <- cohort %>%
    summariseResult(
      group = list("Cohort name" = "cohort_name"), strata = strata,
      variables = variablesToSummary, functions = functionsToSummary,
      minCellCount = minCellCount
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(results_type = "summary characteristics")

  # style intersects
  x <- c(tableIntersect, cohortIntersect)
  patternNames <- lapply(names(x), function(xx) {
    tidyr::expand_grid(
      value = x[[xx]][["value"]], window_name = names(x[[xx]][["window"]])
    ) %>%
      dplyr::mutate(group_name = .env$xx)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      pattern = paste0("_", .data$value, "_", .data$window_name),
      name = gsub("_", " ", paste(
        .data$group_name, .data$value, "in", dplyr::if_else(
          substr(.data$window_name, 1, 1) == "m" &
            suppressWarnings(!is.na(as.numeric(substr(
              .data$window_name, 2, 2)
            ))),
          gsub("m", "-", paste(.data$window_name, "days")), .data$window_name
      )))
    )
  variables %>%
    dplyr::filter(!is.na(.data$group_name)) %>%
    tidyr::separate(
      col = "variable", sep = c("_count_|_flag_|_time_|_count_"),
      into = c("variable", "window_name")
    ) %>%
    dplyr::
  if (length(columNamesCovariates) > 0) {
    for (k in seq_along(covariates)) {
      covariatesColumns <- columNamesCovariates[
        grepl(paste0("_", names(covariates)[k], "_"), columNamesCovariates)
      ]
      results <- results %>%
        dplyr::mutate(
          window_name = dplyr::if_else(
            .data$variable %in% .env$covariatesColumns,
            extractWindowName(.data$variable, names(covariates)[k]),
            dplyr::if_else(
              length(columNamesVisits) > 0, .data$window_name, NA_character_
            )
          ),
          variable_type = dplyr::if_else(
            .data$variable %in% .env$covariatesColumns,
            extractCohortName(.data$variable, names(covariates)[k]),
            .data$variable_type
          )
       )
    }
  }

  # select variables
  results <- results %>%
    dplyr::select(dplyr::any_of(c(
      "cdm_name", "generated_by", "group_name", "group_level", "strata_name",
      "strata_level", "variable", "variable_level", "window_name",
      "estimate_type", "estimate"
    )))

  return(results)
}

addWindowName <- function(window) {
  if (!is.list(window)) {
    window <- list(window)
  }
  if (length(window) > 0) {

  }
}
