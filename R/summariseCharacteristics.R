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
#' @param demographics Whether to summarise demographics data.
#' @param ageGroup A list of age groups.
#' @param tableIntersect A list of arguments that uses addTableIntersect
#' function to add variables to summarise
#' @param cohortIntersect A list of arguments that uses addCohortIntersect
#' function to add variables to summarise.
#' @param conceptIntersect A list of arguments that uses addConceptIntersect
#' function to add variables to summarise
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
                                     demographics = TRUE,
                                     ageGroup = NULL,
                                     tableIntersect = list(),
                                     cohortIntersect = list(),
                                     conceptIntersect = list(),
                                     minCellCount = 5) {
  # check initial tables
  checkX(cohort)
  checkmate::assertLogical(demographics, any.missing = FALSE, len = 1)
  checkCdm(cdm)
  checkStrata(strata, cohort)
  checkAgeGroup(ageGroup)
  checkmate::assertIntegerish(minCellCount, lower = 1)
  checkTableIntersect(tableIntersect, cdm)
  checkCohortIntersect(cohortIntersect, cdm)
  checkConceptIntersect(conceptIntersect, cdm)

  # functions
  functions <- list(
    date = c("median", "min", "q25", "q75", "max", "missing"),
    numeric = c(
      "min", "q05", "q25", "median", "q75", "q95", "max", "mean", "sd",
      "missing"
    ),
    categorical = c("count", "percentage"),
    binary = c("count", "percentage", "missing")
  )

  # select necessary variables
  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", dplyr::all_of(unique(unlist(strata)))
    )

  dic <- dplyr::tibble(full_name = character(), short_name = character())
  variables <- list()
  options(unique_variable_name = 0)

  # demographics
  if (demographics) {
    if (!is.null(ageGroup)) {
      # default names
      ageGroup <- checkAgeGroup(ageGroup)

      # update names
      newNames <- uniqueVariableName(length(ageGroup))
      dic <- dic %>%
        dplyr::union_all(dplyr::tibble(
          full_name = names(ageGroup), short_name = newNames
        ))
      names(ageGroup) <- newNames
    }

    # add demographics
    cohort <- cohort %>%
      addDemographics(ageGroup = ageGroup)

    # update summary settings
    variables <- updateVariables(
      variables = variables,
      date = c("cohort_start_date", "cohort_end_date"),
      numeric = c("prior_observation", "future_observation", "age"),
      categorical = c("sex", newNames)
    )
  }

  # tableIntersect
  for (k in seq_along(tableIntersect)) {
    # prepare arguments
    arguments <- formals(addIntersect)
    arguments <- updateArguments(arguments, tableIntersect[[k]])
    shortNames <- uniqueVariableName(length(arguments$window))
    fullNames <- names(arguments$window)
    names(arguments$window) <- shortNames

    # update dictionary
    addDic <- updateDic(tableIntersect[[k]]$value, shortNames, fullNames)
    dic <- dic %>% dplyr::union_all(addDic)

    # TODO to implement addTableIntersect
    # add intersect
    cohort <- cohort %>%
      PatientProfiles::addIntersect(
        cdm = cdm,
        tableName = arguments$tableName,
        value = arguments$value,
        filterVariable = arguments$filterVariable,
        filterId = arguments$filterId,
        idName = arguments$idName,
        window = arguments$window,
        indexDate = arguments$indexDate,
        censorDate = arguments$censorDate,
        #targetStartDate = arguments$targetStartDate,
        #targetEndDate = arguments$targetEndDate,
        order = arguments$order,
        nameStyle = "{value}_{window_name}"
      )

    # update summary settings
    variables <- updateVariables(
      variables = variables,
      date = addDic$short_name[grepl("date_", addDic$short_name)],
      numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
      binary = addDic$short_name[grepl("flag_", addDic$short_name)]
    )
  }

  # cohortIntersect
  for (k in seq_along(cohortIntersect)) {
    # prepare arguments
    arguments <- formals(addCohortIntersect)
    arguments <- updateArguments(arguments, cohortIntersect[[k]], TRUE)

    # rename windows
    fullNamesWindow <- names(arguments$window)
    shortNamesWindow <- uniqueVariableName(length(arguments$window))
    names(arguments$window) <- shortNamesWindow

    # rename cohorts
    fullNamesCohort <- CDMConnector::cohortSet(
      cdm[[arguments$targetCohortTable]]
    ) %>%
      dplyr::pull("cohort_name")
    shortNamesCohort <- uniqueVariableName(length(fullNamesCohort))
    attr(cdm[[arguments$targetCohortTable]], "cohort_set") <-
      attr(cdm[[arguments$targetCohortTable]], "cohort_set") %>%
      dplyr::rename("old_cohort_name" = "cohort_name") %>%
      dplyr::inner_join(
        dplyr::tibble(
          "old_cohort_name" = fullNamesCohort, "cohort_name" = shortNamesCohort
        ),
        by = "old_cohort_name",
        copy = TRUE
      ) %>%
      CDMConnector::computeQuery()

    # update dictionary
    addDic <- updateDic(
      cohortIntersect[[k]]$value, shortNamesWindow, fullNamesWindow,
      shortNamesCohort, fullNamesCohort
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      PatientProfiles::addCohortIntersect(
        cdm = cdm,
        targetCohortTable = arguments$targetCohortTable,
        targetCohortId = arguments$targetCohortId,
        indexDate = arguments$indexDate,
        censorDate = arguments$censorDate,
        targetStartDate = arguments$targetStartDate,
        targetEndDate = arguments$targetEndDate,
        window = arguments$window,
        order = arguments$order,
        flag = arguments$flag,
        count = arguments$count,
        date = arguments$date,
        days = arguments$days,
        nameStyle = "{value}_{cohort_name}_{window_name}"
      )

    # update summary settings
    variables <- updateVariables(
      variables = variables,
      date = addDic$short_name[grepl("date_", addDic$short_name)],
      numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
      binary = addDic$short_name[grepl("flag_", addDic$short_name)]
    )
  }

  # conceptIntersect
  for (k in seq_along(conceptIntersect)) {
    # prepare arguments
    arguments <- formals(addConceptIntersect)
    arguments <- updateArguments(arguments, conceptIntersect[[k]], TRUE)

    # rename windows
    fullNamesWindow <- names(argumnets$window)
    shortNamesWindow <- uniqueVariableName(length(arguments$window))
    names(argumnets$window) <- shortNamesWindow

    # rename cohorts
    fullNamesConcept <- names(conceptSet)
    shortNamesConcept <- uniqueVariableName(length(fullNamesConcept))
    names(conceptSet) <- shortNamesConcept

    # update dictionary
    addDic <- updateDic(
      conceptIntersect[[k]]$value, shortNamesWindow, fullNamesWindow,
      shortNamesConcept, fullNamesConcept
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      PatientProfiles::addConceptIntersect(
        cdm = cdm,
        conceptSet = arguments$conceptSet,
        indexDate = arguments$indexDate,
        censorDate = arguments$censorDate,
        window = arguments$window,
        targetStartDate = arguments$targetStartDate,
        targetEndDate = arguments$targetEndDate,
        order = arguments$order,
        flag = arguments$flag,
        count = arguments$count,
        date = arguments$date,
        days = arguments$days,
        nameStyle = "{value}_{concept_name}_{window_name}"
      )

    # update summary settings
    variables <- updateVariables(
      variables = variables,
      date = addDic$short_name[grepl("date_", addDic$short_name)],
      numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
      binary = addDic$short_name[grepl("flag_", addDic$short_name)]
    )
  }

  # update cohort_names
  cohort <- cohort %>% addCohortName()

  # summarise results
  results <- cohort %>%
    summariseResult(
      group = list("cohort_name"),
      strata = strata,
      variables = variables,
      functions = functions,
      minCellCount = minCellCount
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(result_type = "Summary characteristics")

  # rename variables
  results <- results %>%
    dplyr::left_join(
      dic %>% dplyr::rename("variable" = "short_name"),
      by = "variable"
    ) %>%
    dplyr::mutate("full_name" = dplyr::if_else(
      is.na(.data$full_name), .data$variable, .data$full_name
    )) %>%
    dplyr::select(-"variable") %>%
    dplyr::rename("variable" = "full_name")

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

uniqueVariableName <- function(n = 1) {
  if (n != 0) {
    i <- getOption("unique_variable_name", 0) + 1:n
    options(unique_variable_name = i[length(i)])
    x <- sprintf("variable_%05i", i)
  } else {
    x <- NULL
  }
  return(x)
}
updateVariables <- function(variables,
                            date = NULL,
                            numeric = NULL,
                            binary = NULL,
                            categorical = NULL) {
  variables$date <- c(variables$date, date)
  variables$numeric <- c(variables$numeric, numeric)
  variables$binary <- c(variables$binary, binary)
  variables$categorical <- c(variables$categorical, categorical)
  return(variables)
}
updateArguments <- function(arguments, settings, splitValues = FALSE) {
  if (!is.list(settings[["window"]])) {
    settings[["window"]] <- list(settings[["window"]])
  }
  for (nm in names(settings)) {
    arguments[[nm]] <- settings[[nm]]
  }
  if (splitValues) {
    arguments$flag <- FALSE
    arguments$count <- FALSE
    arguments$date <- FALSE
    arguments$days <- FALSE
    arguments[arguments$value] <- TRUE
  }
  names(arguments[["window"]]) <- getWindowNames(arguments[["window"]])
  return(arguments)
}
updateDic <- function(value,
                      shortNames1,
                      fullNames1,
                      shortNames2 = NULL,
                      fullNames2 = NULL) {
  x <- expand.grid("value" = value, "short_name1" = shortNames1) %>%
    dplyr::as_tibble() %>%
    dplyr::inner_join(
      dplyr::tibble("short_name1" = shortNames1, "full_name1" = fullNames1),
      by = "short_name1"
    )
  if (!is.null(shortNames2)) {
    x <- x %>%
      dplyr::inner_join(
        expand.grid("short_name1" = shortNames1, "short_name2" = shortNames2) %>%
          dplyr::as_tibble(),
        by = "short_name1"
      ) %>%
      dplyr::inner_join(
        dplyr::tibble("short_name2" = shortNames2, "full_name2" = fullNames2),
        by = "short_name2"
      ) %>%
      dplyr::mutate(
        "short_name" = paste0(
          .data$value, "_", .data$short_name2, "_", .data$short_name1
        ),
        "full_name" = paste0(
          .data$value, "_", .data$full_name2, "_", .data$full_name1
        )
      )
  } else {
    x <- x %>%
      dplyr::mutate(
        "short_name" = paste0(.data$value, "_", .data$short_name1),
        "full_name" = paste0(.data$value, "_", .data$full_name1)
      )
  }
  x %>%
    dplyr::select("short_name", "full_name")
}
