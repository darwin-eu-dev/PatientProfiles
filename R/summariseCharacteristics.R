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
#' function to add variables to summarise.
#' @param otherVariables Other variables contained in cohort that you want to be
#' summarised.
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
                                     otherVariables = character()) {
  # check initial tables
  checkX(cohort)
  checkmate::assertLogical(demographics, any.missing = FALSE, len = 1)
  checkCdm(cdm)
  checkStrata(strata, cohort)
  checkAgeGroup(ageGroup)
  checkTableIntersect(tableIntersect, cdm)
  checkCohortIntersect(cohortIntersect, cdm)
  checkConceptIntersect(conceptIntersect, cdm)
  checkOtherVariables(otherVariables, cohort)

  # check empty
  if (demographics == FALSE &
      length(tableIntersect) == 0 &
      length(cohortIntersect) == 0 &
      length(conceptIntersect) == 0 ) {
    cli::cli_abort(
      "Please fill demographics, tableIntersect, cohortIntersect or
      conceptIntersect"
    )
  }

  # functions
  functions <- list(
    date = c("min", "q05", "q25", "median", "q75", "q95", "max"),
    numeric = c(
      "min", "q05", "q25", "median", "q75", "q95", "max", "mean", "sd"
    ),
    categorical = c("count", "percentage"),
    binary = c("count", "percentage")
  )

  # select necessary variables
  cohort <- cohort %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", dplyr::all_of(unique(unlist(strata))),
      dplyr::all_of(otherVariables)
    )

  if (cohort %>% dplyr::tally() %>% dplyr::pull() == 0) {
    if (any(c("subject_id", "person_id") %in% colnames(cohort))) {
      variables <- c("number subjects", "number records")
    } else {
      variables <- "number records"
    }
    result <- dplyr::tibble(
      "cdm_name" = CDMConnector::cdmName(cdm),
      "result_type" = "summarised_characteristics",
      "package_name" = "PatientProfiles",
      "package_version" = as.character(utils::packageVersion("PatientProfiles")),
      "group_name" = "overall",
      "group_level" = "overall",
      "strata_name" = "overall",
      "strata_level" = "overall",
      "variable_name" = variables,
      "variable_level" = as.character(NA),
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "estimate_value" = "0",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
      omopgenerics::summarisedResult()
    return(result)
  }

  dic <- dplyr::tibble(
    full_name = character(), short_name = character(), value = character(),
    cohort = character(), window = character(), variable_group = character()
  )
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
          full_name = names(ageGroup), short_name = newNames,
          value = as.character(NA), cohort = as.character(NA),
          window = as.character(NA), variable_group = as.character(NA)
        ))
      names(ageGroup) <- newNames
      demographicsCategorical <- c("sex", newNames)
    } else {
      demographicsCategorical <- "sex"
    }

    # add demographics
    cohort <- cohort %>%
      addDemographics(ageGroup = ageGroup)

    # update summary settings
    variables <- updateVariables(
      variables = variables,
      date = c("cohort_start_date", "cohort_end_date"),
      numeric = c("prior_observation", "future_observation", "age"),
      categorical = demographicsCategorical
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
    addDic <- updateDic(
      tableIntersect[[k]]$value, shortNames, fullNames, arguments$tableName,
      arguments$tableName, names(tableIntersect)[k]
    )
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
        nameStyle = paste0("{value}_", arguments$tableName, "_{window_name}")
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
    )
    if (!is.null(arguments$targetCohortId)) {
      fullNamesCohort <- fullNamesCohort %>%
        dplyr::filter(
          .data$cohort_definition_id %in% !!arguments$targetCohortId
        )
    }
    fullNamesCohort <- fullNamesCohort %>% dplyr::pull("cohort_name")
    shortNamesCohort <- uniqueVariableName(length(fullNamesCohort))

    # update cohort_set
    originalCohortSet <- attr(cdm[[arguments$targetCohortTable]], "cohort_set")
    newCohortSet <- originalCohortSet %>%
      dplyr::rename(old_cohort_name = "cohort_name") %>%
      dplyr::inner_join(
        dplyr::tibble(
          old_cohort_name = fullNamesCohort, cohort_name = shortNamesCohort
        ),
        by = "old_cohort_name",
        copy = TRUE
      ) %>%
      dplyr::compute()
    attr(cdm[[arguments$targetCohortTable]], "cohort_set") <- newCohortSet

    # update dictionary
    addDic <- updateDic(
      cohortIntersect[[k]]$value, shortNamesWindow, fullNamesWindow,
      shortNamesCohort, fullNamesCohort, names(cohortIntersect)[k]
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

    # restore cohort_set
    attr(cdm[[arguments$targetCohortTable]], "cohort_set") <- originalCohortSet
  }

  # conceptIntersect
  for (k in seq_along(conceptIntersect)) {
    # prepare arguments
    arguments <- formals(addConceptIntersect)
    arguments <- updateArguments(arguments, conceptIntersect[[k]], TRUE)

    # rename windows
    fullNamesWindow <- names(arguments$window)
    shortNamesWindow <- uniqueVariableName(length(arguments$window))
    names(arguments$window) <- shortNamesWindow

    # rename cohorts
    fullNamesConcept <- names(arguments$conceptSet)
    shortNamesConcept <- uniqueVariableName(length(fullNamesConcept))
    names(arguments$conceptSet) <- shortNamesConcept

    # update dictionary
    addDic <- updateDic(
      conceptIntersect[[k]]$value, shortNamesWindow, fullNamesWindow,
      shortNamesConcept, fullNamesConcept, names(conceptIntersect)[k]
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      PatientProfiles::addConceptIntersect(
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

  # detect other variables
  x <- variableTypes(cohort %>% dplyr::select(dplyr::all_of(otherVariables)))
  variables <- updateVariables(
    variables = variables,
    date = x %>% dplyr::filter(.data$variable_type == "date") %>% dplyr::pull("variable"),
    numeric = x %>% dplyr::filter(.data$variable_type == "numeric") %>% dplyr::pull("variable"),
    binary = x %>% dplyr::filter(.data$variable_type == "binary") %>% dplyr::pull("variable"),
    categorical = x %>% dplyr::filter(.data$variable_type == "categorical") %>% dplyr::pull("variable")
  )

  # summarise results
  results <- cohort %>%
    summariseResult(
      group = list("cohort_name"),
      strata = strata,
      variables = variables,
      functions = functions[names(variables)]
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(result_type = "summarised_characteristics")

  # rename variables
  results <- results %>%
    dplyr::left_join(
      tidyDic(dic) |> dplyr::rename("variable_name" = "variable"),
      by = "variable_name"
    ) %>%
    dplyr::mutate(
      "variable_name" = dplyr::if_else(
        is.na(.data$new_variable), .data$variable_name, .data$new_variable
      ),
      "variable_level" = dplyr::if_else(
        is.na(.data$new_variable_level), .data$variable_level,
        .data$new_variable_level
      )
    ) %>%
    dplyr::select(-"new_variable", -"new_variable_level") %>%
    dplyr::mutate(dplyr::across(
      c("variable_name", "variable_level"),
      ~ stringr::str_to_sentence(gsub("_", " ", .x))
    )) %>%
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns("summarised_result"))) |>
    dplyr::as_tibble() |>
    omopgenerics::summarisedResult()

  return(results)
}

tidyDic <- function(dic) {
  dic %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate("window" = gsub("_", " ", .data$window)) %>%
    tidyr::separate_wider_delim(
      cols = "window", delim = " ", names = "win", too_few = "align_start",
      too_many = "drop", cols_remove = FALSE
    ) %>%
    dplyr::mutate("window" = dplyr::if_else(
      substr(.data$win, 1, 1) == "m" &
        suppressWarnings(!is.na(as.numeric(substr(.data$win, 2, nchar(.data$win))))),
      gsub("m", "-", .data$window),
      .data$window
    )) %>%
    tidyr::separate_wider_delim(
      cols = "window", delim = " ", names = c("w1", "w2", "w3"),
      too_few = "align_start", too_many = "drop", cols_remove = FALSE
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("w1", "w3")),
      ~ suppressWarnings(as.numeric(.x))
    )) %>%
    dplyr::mutate("window" = dplyr::if_else(
      !is.na(.data$w1) & .data$w2 == "to" & !is.na(.data$w3),
      paste(
        "from",
        dplyr::if_else(is.infinite(.data$w1), "any time prior", as.character(.data$w1)),
        "to",
        dplyr::if_else(is.infinite(.data$w3), "any time after", as.character(.data$w3))
      ),
      .data$window
    )) %>%
    dplyr::mutate("new_variable" = paste(
      .data$variable_group, .data$value, .data$window
    )) %>%
    dplyr::select(
      "variable" = "short_name", "new_variable", "new_variable_level" = "cohort"
    ) %>%
    dplyr::union_all(
      dic %>%
        dplyr::filter(is.na(.data$value)) %>%
        dplyr::mutate("new_variable_level" = as.character(NA)) %>%
        dplyr::select(
          "variable" = "short_name", "new_variable" = "full_name",
          "new_variable_level"
        )
    )
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
                      shortWindow,
                      fullWindow,
                      shortCohort,
                      fullCohort,
                      variableGroup) {
  expand.grid("value" = value, "short_window" = shortWindow) %>%
    dplyr::as_tibble() %>%
    dplyr::inner_join(
      dplyr::tibble("short_window" = shortWindow, "full_window" = fullWindow),
      by = "short_window"
    ) %>%
      dplyr::inner_join(
        expand.grid("short_window" = shortWindow, "short_cohort" = shortCohort) %>%
          dplyr::as_tibble(),
        by = "short_window"
      ) %>%
      dplyr::inner_join(
        dplyr::tibble("short_cohort" = shortCohort, "full_cohort" = fullCohort),
        by = "short_cohort"
      ) %>%
      dplyr::mutate(
        "short_name" = paste0(
          .data$value, "_", .data$short_cohort, "_", .data$short_window
        ),
        "full_name" = paste0(
          .data$value, "_", .data$full_cohort, "_", .data$full_window
        )
      ) %>%
    dplyr::select(
      "short_name", "full_name", "value", "cohort" = "full_cohort",
      "window" = "full_window"
    ) %>%
    dplyr::mutate("variable_group" = .env$variableGroup)
}
