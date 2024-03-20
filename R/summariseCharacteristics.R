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
#' @param cohort A cohort in the cdm.
#' @param cdm A cdm reference.
#' @param strata Stratification list.
#' @param demographics Whether to summarise demographics data.
#' @param ageGroup A list of age groups.
#' @param tableIntersect A list of arguments that uses addTableIntersect
#' function to add variables to summarise.
#' @param cohortIntersect A list of arguments that uses addCohortIntersect
#' function to add variables to summarise.
#' @param conceptIntersect A list of arguments that uses addConceptIntersect
#' function to add variables to summarise.
#' @param otherVariables Other variables contained in cohort that you want to be
#' summarised.
#'
#' @return A summary of the characteristics of the individuals.
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
#'     "Number visits prior year" = list(
#'       tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#'     )
#'   ),
#'   cohortIntersect = list(
#'     "Drugs prior year" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'     ),
#'     "Conditions any time prior" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-Inf, -1)
#'     )
#'   )
#' )
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
summariseCharacteristics <- function(cohort,
                                     cdm = lifecycle::deprecated(),
                                     strata = list(),
                                     demographics = TRUE,
                                     ageGroup = NULL,
                                     tableIntersect = list(),
                                     cohortIntersect = list(),
                                     conceptIntersect = list(),
                                     otherVariables = character()) {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "summariseCharacteristics(cdm)")
  }
  # check initial tables
  cdm <- omopgenerics::cdmReference(cohort)
  checkX(cohort)
  checkmate::assertLogical(demographics, any.missing = FALSE, len = 1)
  checkCdm(cdm)
  if (!is.list(strata)) {
    strata <- list(strata)
  }
  checkStrata(strata, cohort)
  checkAgeGroup(ageGroup)
  tableIntersect <- checkTableIntersect(tableIntersect, cdm)
  cohortIntersect <- checkCohortIntersect(cohortIntersect, cdm)
  conceptIntersect <- checkConceptIntersect(conceptIntersect, cdm)
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
      omopgenerics::newSummarisedResult()
    return(result)
  }

  dic <- dplyr::tibble(
    short_name = character(), new_variable_name = character(),
    new_variable_level = character(), table = character(), window = character()
  )
  variables <- list()

  # demographics
  if (demographics) {
    cli::cli_alert_info("adding demographics columns")

    sex <- uniqueVariableName()
    age <- uniqueVariableName()
    priorObservation <- uniqueVariableName()
    futureObservation <- uniqueVariableName()
    demographicsCategorical <- sex

    if (!is.null(ageGroup)) {
      # default names
      ageGroup <- checkAgeGroup(ageGroup)

      # update names
      newNames <- uniqueVariableName(length(ageGroup))
      dic <- dic %>%
        dplyr::union_all(dplyr::tibble(
          short_name = newNames,
          new_variable_name = names(ageGroup),
          new_variable_level = as.character(NA),
          table = as.character(NA),
          window = as.character(NA)
        ))
      names(ageGroup) <- newNames
      demographicsCategorical <- c(demographicsCategorical, newNames)
    }
    dic <- dic %>%
      dplyr::union_all(dplyr::tibble(
        short_name = c(sex, age, priorObservation, futureObservation),
        new_variable_name = c(
          "sex", "age", "prior_observation", "future_observation"
        ),
        new_variable_level = as.character(NA),
        table = as.character(NA),
        window = as.character(NA)
      ))

    # add demographics
    cohort <- cohort %>%
      addDemographics(
        ageGroup = ageGroup,
        sexName = sex,
        ageName = age,
        priorObservationName = priorObservation,
        futureObservationName = futureObservation
      )

    # update summary settings
    variables <- variables |>
      updateVariables(
        date = c("cohort_start_date", "cohort_end_date"),
        numeric = c(priorObservation, futureObservation, age),
        categorical = demographicsCategorical
      )
  }

  # tableIntersect
  for (k in seq_along(tableIntersect)) {
    cli::cli_alert_info(
      "adding table intersect columns for table: {tableIntersect[[k]]$tableName}"
    )
    # prepare arguments
    arguments <- formals(.addTableIntersect)
    arguments <- updateArguments(arguments, tableIntersect[[k]])
    shortNames <- uniqueVariableName(length(arguments$window))
    fullNames <- names(arguments$window)
    names(arguments$window) <- shortNames

    # update dictionary
    addDic <- updateDic(
      tableIntersect[[k]]$value, shortNames, fullNames,
      arguments$tableName, paste("any", arguments$tableName),
      arguments$tableName
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      .addTableIntersect(
        tableName = arguments$tableName,
        window = arguments$window,
        indexDate = arguments$indexDate,
        censorDate = arguments$censorDate,
        order = arguments$order,
        flag = arguments$flag,
        count = arguments$count,
        date = arguments$date,
        days = arguments$days,
        field = arguments$field,
        overlap = arguments$overlap,
        nameStyle = "{value}_{table_name}_{window_name}"
      )

    # update summary settings
    if (length(arguments$field) > 0) {
      cate <- addDic$short_name[grepl(arguments$field, addDic$short_name)]
    } else {
      cate <- NULL
    }
    variables <- variables |>
      updateVariables(
        date = addDic$short_name[grepl("date_", addDic$short_name)],
        numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
        binary = addDic$short_name[grepl("flag_", addDic$short_name)],
        categorical = cate
      )
  }

  # cohortIntersect
  for (k in seq_along(cohortIntersect)) {
    cli::cli_alert_info(
      "adding cohort intersect columns for table: {cohortIntersect[[k]]$targetCohortTable}"
    )
    # prepare arguments
    arguments <- formals(.addCohortIntersect)
    arguments <- updateArguments(arguments, cohortIntersect[[k]])

    # rename windows
    fullNamesWindow <- names(arguments$window)
    shortNamesWindow <- uniqueVariableName(length(arguments$window))
    names(arguments$window) <- shortNamesWindow

    # rename cohorts
    fullNamesCohort <- omopgenerics::settings(
      cdm[[arguments$targetCohortTable]]
    ) |>
      dplyr::pull("cohort_name")
    shortNamesCohort <- uniqueVariableName(length(fullNamesCohort))

    # update cohort_set
    originalCohortSet <- omopgenerics::settings(cdm[[arguments$targetCohortTable]])
    newCohortSet <- originalCohortSet %>%
      dplyr::select("cohort_definition_id", "cohort_name") |>
      dplyr::rename(old_cohort_name = "cohort_name") %>%
      dplyr::inner_join(
        dplyr::tibble(
          old_cohort_name = fullNamesCohort, cohort_name = shortNamesCohort
        ),
        by = "old_cohort_name"
      ) |>
      dplyr::select(-"old_cohort_name")
    cdm[[arguments$targetCohortTable]] <- cdm[[arguments$targetCohortTable]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = newCohortSet, cohortAttritionRef = NULL
      )

    if (!is.null(arguments$targetCohortId)) {
      id <- originalCohortSet |>
        dplyr::filter(.data$cohort_definition_id %in% arguments$targetCohortId) |>
        dplyr::pull("cohort_name")
      id <- which(id %in% fullNamesCohort)
      fullNamesCohort <- fullNamesCohort[id]
      shortNamesCohort <- shortNamesCohort[id]
    }

    # update dictionary
    addDic <- updateDic(
      cohortIntersect[[k]]$value, shortNamesWindow, fullNamesWindow,
      shortNamesCohort, fullNamesCohort, arguments$targetCohortTable
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      .addCohortIntersect(
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
    variables <- variables |>
      updateVariables(
        date = addDic$short_name[grepl("date_", addDic$short_name)],
        numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
        binary = addDic$short_name[grepl("flag_", addDic$short_name)]
      )

    # restore cohort_set
    cdm[[arguments$targetCohortTable]] <- cdm[[arguments$targetCohortTable]] |>
      omopgenerics::newCohortTable(
        cohortSetRef = originalCohortSet, cohortAttritionRef = NULL
      )
  }

  # conceptIntersect
  for (k in seq_along(conceptIntersect)) {
    cli::cli_alert_info(
      "adding concept intersect columns for conceptSet {k}/{length(conceptIntersect)}"
    )
    # prepare arguments
    arguments <- formals(.addConceptIntersect)
    arguments <- updateArguments(arguments, conceptIntersect[[k]])

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
      shortNamesConcept, fullNamesConcept, NA_character_
    )
    dic <- dic %>% dplyr::union_all(addDic)

    # add intersect
    cohort <- cohort %>%
      .addConceptIntersect(
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
    variables <- variables |>
      updateVariables(
        date = addDic$short_name[grepl("date_", addDic$short_name)],
        numeric = addDic$short_name[grepl("count_|time_", addDic$short_name)],
        binary = addDic$short_name[grepl("flag_", addDic$short_name)],
        categorical = addDic$short_name[
          !grepl("flag_|count_|time_|date_", addDic$short_name)
        ]
      )
  }

  # update cohort_names
  cohort <- cohort %>% addCohortName()

  # detect other variables
  x <- variableTypes(cohort %>% dplyr::select(dplyr::all_of(otherVariables)))
  datesVariables <- x %>%
    dplyr::filter(.data$variable_type == "date") %>%
    dplyr::pull("variable_name")
  numericVariables <- x %>%
    dplyr::filter(.data$variable_type %in% c("numeric", "integer")) %>%
    dplyr::pull("variable_name")
  binaryVariables <- numericVariables[
    lapply(numericVariables, function(x) {
      cohort |>
        dplyr::select(dplyr::all_of(x)) |>
        dplyr::distinct() |>
        dplyr::pull() |>
        binaryVariable()
    }) |>
      unlist()
  ]
  numericVariables  <- numericVariables[!numericVariables %in% binaryVariables]
  categoricalVariables <- x %>%
    dplyr::filter(.data$variable_type == "categorical") %>%
    dplyr::pull("variable_name")
  variables <- variables |>
    updateVariables(
      date = datesVariables,
      numeric = numericVariables,
      binary = binaryVariables,
      categorical = categoricalVariables
    )

  variables <- variables[lengths(variables) > 0]

  cli::cli_alert_info("summarising data")
  # summarise results
  results <- cohort %>%
    summariseResult(
      group = list("cohort_name"),
      strata = strata,
      variables = variables,
      estimates = functions[names(variables)],
      verbose = FALSE
    ) %>%
    addCdmName(cdm = cdm) %>%
    dplyr::mutate(result_type = "summarised_characteristics")

  # rename variables
  results <- results %>%
    dplyr::left_join(
      dic |> dplyr::rename("variable_name" = "short_name"),
      by = "variable_name"
    ) %>%
    dplyr::mutate(
      "variable_name" = dplyr::if_else(
        is.na(.data$new_variable_name),
        .data$variable_name,
        .data$new_variable_name
      ),
      "variable_level" = dplyr::if_else(
        is.na(.data$new_variable_level),
        .data$variable_level,
        .data$new_variable_level
      )
    ) %>%
    dplyr::select(-c(
      "new_variable_name", "new_variable_level", "additional_name",
      "additional_level"
    )) %>%
    dplyr::mutate(dplyr::across(
      c("variable_name", "variable_level"),
      ~ stringr::str_to_sentence(gsub("_", " ", .x))
    )) %>%
    dplyr::mutate(
      "additional_name" = dplyr::case_when(
        is.na(.data$table) & is.na(.data$window) ~ "overall",
        !is.na(.data$table) & is.na(.data$window) ~ "table",
        is.na(.data$table) & !is.na(.data$window) ~ "window",
        !is.na(.data$table) & !is.na(.data$window) ~ "table and window"
      ),
      "additional_level" = dplyr::case_when(
        is.na(.data$table) & is.na(.data$window) ~ "overall",
        !is.na(.data$table) & is.na(.data$window) ~ .data$table,
        is.na(.data$table) & !is.na(.data$window) ~ .data$window,
        !is.na(.data$table) & !is.na(.data$window) ~ paste(
          .data$table, "and", .data$window
        )
      )
    ) |>
    dplyr::select(-dplyr::any_of(c("table", "window"))) |>
    dplyr::as_tibble()

  # correct integers
  integers <- results$estimate_value
  integers[results$estimate_type == "date"] <- NA
  integers <- as.numeric(integers)
  results$estimate_type[
    results$estimate_type == "numeric" & integers == floor(integers)
  ] <- "integer"

  results <- omopgenerics::newSummarisedResult(results)

  cli::cli_alert_success("summariseCharacteristics finished!")

  return(results)
}

#' Summarise concept intersect with a cohort_table
#'
#' @param cohort A cohort in the cdm
#' @param strata Stratification list
#' @param conceptIntersect A list of arguments that uses addConceptIntersect
#' function to add variables to summarise.
#'
#' @return A summary of the concept intersect of the individuals
#'
#' @export
#'
summariseConceptIntersect <- function(cohort,
                                      conceptIntersect,
                                      strata = list()) {
  if (length(conceptIntersect) == 0) {
    cli::cli_abort("Please provide at least one cocneptSet to insersect with.")
  }
  summariseCharacteristics(
    cohort = cohort, strata = strata, demographics = FALSE, ageGroup = NULL,
    tableIntersect = list(), cohortIntersect = list(),
    conceptIntersect = conceptIntersect, otherVariables = character()
  )
}

correctWindowName <- function(windowName) {
  id <- grepl("_to_", windowName)
  windowName[id] <- windowName[id] |>
    strsplit("_to_") |>
    lapply(function(x) {
      if (length(x) == 2) {
        idd <- startsWith(x = x, prefix = "m")
        x[idd] <- paste0("-", substr(x[idd], 2, nchar(x[idd])))
        x <- paste0(x, collapse = " to ")
      } else {
        x <- paste0(x, collapse = "_to_")
      }
      return(x)
    }) |>
    unlist()
  return(windowName)
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
updateArguments <- function(arguments, provided) {
  for (nm in names(provided)) {
    arguments[[nm]] <- provided[[nm]]
  }
  if (!is.list(arguments[["window"]])) {
    arguments[["window"]] <- list(arguments[["window"]])
  }
  values <- c("flag", "count", "date", "days")
  arguments$flag <- FALSE
  arguments$count <- FALSE
  arguments$date <- FALSE
  arguments$days <- FALSE
  id <- arguments$value[arguments$value %in% values]
  if (length(id) > 0) {
    arguments[[id]] <- TRUE
  }
  arguments$field <- arguments$value[!arguments$value %in% values]
  names(arguments$window) <- getWindowNames(arguments$window)
  return(arguments)
}
updateDic <- function(value,
                      shortWindow,
                      fullWindow,
                      shortCohort,
                      fullCohort,
                      tableName) {
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
    dplyr::mutate("short_name" = paste0(
      .data$value, "_", .data$short_cohort, "_", .data$short_window
    )) %>%
    dplyr::mutate(
      "new_variable_name" = dplyr::if_else(
        .data$value %in% c("flag", "count", "date", "days"),
        .data$full_cohort,
        .data$value
      ),
      "new_variable_level" = dplyr::if_else(
        .data$value %in% c("flag", "count", "date", "days"),
        .data$value,
        as.character(NA)
      ),
      "table" = .env$tableName,
      "window" = correctWindowName(.data$full_window)
    ) |>
    dplyr::select(
      "short_name", "new_variable_name", "new_variable_level", "table", "window"
    )
}
