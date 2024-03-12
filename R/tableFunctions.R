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

#' Format a summarised_characteristics object into a visual table.
#'
#' @param result A summarised_characteristics object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param format .
#' @param splitStrata .
#' @param format .
#' @param cdmName .
#' @param cohortName .
#' @param style .
#' @param minCellCount .
#' @param .options See optionsTableCharacteristics() for default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   formatCharacteristics()
#' }
#'
#' @return A tibble with a tidy version of the summarised_characteristics
#' object.
#'
#' @export
#'
formatCharacteristics <- function(result,
                                  type = "gt",
                                  splitStrata = TRUE,
                                  format = c(
                                    "N (%)" = "<count> (<percentage>%)",
                                    "N" = "<count>",
                                    "<median> [<q25> - <q75>]",
                                    "<mean> (<sd>)",
                                    "range" = "<min> to <max>"
                                  ),
                                  cdmName = TRUE,
                                  cohortName = TRUE,
                                  style = "default",
                                  minCellCount = 5,
                                  .options = list()) {
  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "summarised_characteristics")
  checkmate::assertChoice(type, c("gt", "flextable"))
  checkmate::assertLogical(splitStrata, any.missing = FALSE, len = 1)
  checkmate::assertCharacter(format, any.missing = FALSE)
  checkmate::assertLogical(cdmName, any.missing = FALSE, len = 1)
  checkmate::assertLogical(cohortName, any.missing = FALSE, len = 1)
  checkmate::assertList(.options)
  checkmate::assertIntegerish(minCellCount, any.missing = FALSE, len = 1)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  result <- result |>
    suppress(minCellCount = minCellCount) |>
    visOmopResults::formatEstimateValue(
      decimals = .options$decimals,
      decimalMark = .options$decimalMark,
      bigMark = .options$bigMark
    ) |>
    visOmopResults::formatEstimateName(
      estimateNameFormat = format, keepNotFormatted = .options$keepNotFormatted
    ) |>
    dplyr::select(-c("result_type", "package_name", "package_version", "estimate_type")) |>
    visOmopResults::splitGroup() |>
    visOmopResults::splitAdditional()
  if (!"table" %in% colnames(result)) {
    result <- result |> dplyr::mutate("table" = NA_character_)
  }
  if (!"window" %in% colnames(result)) {
    result <- result |> dplyr::mutate("window" = NA_character_)
  }
  result <- result |>
    dplyr::mutate("variable_name" = dplyr::case_when(
      is.na(.data$table) & is.na(.data$window) ~ .data$variable_name,
      is.na(.data$table) & !is.na(.data$window) ~ paste(
        .data$variable_name, "in", .data$window
      ),
      !is.na(.data$table) & is.na(.data$window) ~ paste0(
        .data$variable_name, " [", .data$table, "]"
      ),
      !is.na(.data$table) & !is.na(.data$window) ~ paste0(
        .data$variable_name, " [", .data$table, "] in ", .data$window
      )
    )) |>
    dplyr::select(-c("table", "window"))

  colsStrata <- visOmopResults::strataColumns(result)
  if (length(colsStrata) > 0 & !splitStrata) {
    result <- result |>
      dplyr::mutate(
        "strata" = paste0(.data$strata_name, ": ", .data$strata_level)
      )
    colsStrata <- "strata"
  } else {

    result <- result |>
      visOmopResults::splitStrata()
  }

  headers <- character()
  if (cdmName) headers <- c(headers, "CDM name")
  if (cohortName) headers <- c(headers, "Cohort name")
  if (length(colsStrata) > 0) {
    headers <- c(headers, "strata")
    if (colsStrata != "strata") {
      headers <- c(headers, colsStrata)
    }
  }
  result <- result |>
    dplyr::rename(
      "CDM name" = "cdm_name", "Cohort name" = "cohort_name",
      "Variable" = "variable_name", "Level" = "variable_level",
      "Format" = "estimate_name"
    ) |>
    dplyr::mutate("Cohort name" = stringr::str_to_sentence(
      gsub("_", " ", .data[["Cohort name"]])
    )) |>
    visOmopResults::formatTable(
      header = headers, delim = "\n",
      includeHeaderName = .options$includeHeaderName,
      includeHeaderKey = TRUE
    )


  result <- switch(
    type,
    "gt" = visOmopResults::gtTable(
      x = result, delim = "\n", style = style, na = .options$na,
      title = .options$title, subtitle = .options$subtitle,
      caption = .options$caption, groupNameCol = .options$groupNameCol,
      groupNameAsColumn = .options$groupNameAsColumn,
      groupOrder = .options$groupOrder, colsToMergeRows = c("Variable", "Level")
    ),
    "flextable" = visOmopResults::fxTable(
      x = result, delim = "\n", style = style, na = .options$na,
      title = .options$title, subtitle = .options$subtitle,
      caption = .options$caption, groupNameCol = .options$groupNameCol,
      groupNameAsColumn = .options$groupNameAsColumn,
      groupOrder = .options$groupOrder, colsToMergeRows = c("Variable", "Level")
    )
  )

  return(result)
}

defaultCharacteristicsOptions <- function(.options) {
  defaults <- list(
    "decimals" = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    "decimalMark" = ".",
    "bigMark" = ",",
    "keepNotFormatted" = FALSE,
    "includeHeaderName" = TRUE,
    "na" = "-",
    "title" = NULL,
    "subtitle" = NULL,
    "caption" = NULL,
    "groupNameCol" = NULL,
    "groupNameAsColumn" = FALSE,
    "groupOrder" = NULL
  )

  for (opt in names(.options)) {
    defaults[[opt]] <- .options[[opt]]
  }
  return(defaults)
}

#' Additional arguments for the function formatCharacteristics.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' formatCharacteristics. and their given default values.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableCharacteristics()
#' }
#'
#'
optionsTableCharacteristics <- function() {
  return(defaultCharacteristicsOptions(NULL))
}

#' Format a cohort_overlap object into a visual table.
#'
#' @param result A cohort_overlap object.
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param strataName Names of the strata names to include.
#' @param strataLevel Names of the strata levels to include.
#' @param splitStrata If TRUE strata name-levle columns will be splitted.
#' @param cdmName Name of the databases to include.
#' @param variableName Name of the variable names to include.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", or "tibble".
#' @param minCellCount Counts below which results will be clouded.
#' @param .options named list with additional formatting options.
#' PatientProfiles::optionsTableCohortOverlap() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' }
#'
#' @return A formatted table of the overlap_cohort summarised object.
#'
#' @export

tableCohortOverlap  <- function(result,
                                cohortNameReference = NULL,
                                cohortNameComparator = NULL,
                                strataName = "overall",
                                strataLevel = "overall",
                                splitStrata = TRUE,
                                cdmName = NULL,
                                variableName = c("number records",
                                                 "number subjects"),
                                type = "gt",
                                minCellCount = 5,
                                .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertChoice(type, c("gt", "flextable", "tibble"))
  checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  checkmate::assertCharacter(strataName, null.ok = TRUE)
  checkmate::assertCharacter(strataLevel, null.ok = TRUE)
  checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertCharacter(variableName, null.ok = TRUE)
  checkmate::assertList(.options)

  # split table
  x <- result |>
    visOmopResults::splitGroup()

  # add default values
  cohortNameReference <- defaultColumnSelector(
    cohortNameReference,
    x$cohort_name_reference,
    "cohort_name_reference"
  )
  cohortNameComparator <- defaultColumnSelector(
    cohortNameComparator,
    x$cohort_name_comparator,
    "cohort_name_comparator"
  )
  variableName <- defaultColumnSelector(
    variableName,
    x$variable_name,
    "variable_name"
  )
  strataName <- defaultColumnSelector(
    strataName,
    x$strata_name,
    "strata_name"
  )
  if (is.null(strataLevel)) {
    strataLevel <- unique(x$strata_level[x$strata_name %in% strataName])
  }
  cdmName <- defaultColumnSelector(cdmName, x$cdm_name, "cdm_name")
  .options <- defaultOverlapOptions(.options)

  # format table
  x <- x |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$variable_name == .env$variableName) |>
    dplyr::filter(.data$strata_name %in% .env$strataName) |>
    dplyr::filter(.data$strata_level %in% .env$strataLevel) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    getTidyOverlap() |>
    dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    # to support header in visOmopResults 0.1.0
    dplyr::mutate(
      total = .data$reference + .data$comparator,
      reference = .data$reference - .data$overlap,
      comparator = .data$comparator - .data$overlap
    ) |>
    dplyr::mutate(
      only_in_reference = dplyr::if_else(
        (.data$reference < .env$minCellCount) | is.na(.data$reference),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$reference, .data$reference/.data$total * 100, .env$.options)
      ),
      only_in_comparator = dplyr::if_else(
        (.data$comparator < .env$minCellCount) | is.na(.data$comparator),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$comparator, .data$comparator/.data$total * 100, .env$.options)
      ),
      overlap = dplyr::if_else(
        (.data$overlap < .env$minCellCount) | is.na(.data$overlap),
        paste0("<",niceNum(.env$minCellCount, .options, "integer")),
        formatOverlapEstimate(.data$overlap, .data$overlap*2/.data$total * 100, .env$.options)),
      estimate_name = "N (%)"
    ) |>
    dplyr::select(c("cdm_name", "cohort_name_reference", "cohort_name_comparator",
                    "strata_name", "strata_level", "variable_name", "estimate_name",
                    "only_in_reference", "only_in_comparator", "overlap"))

  # split strata
  if (splitStrata) {
    x <- x |>
      visOmopResults::splitStrata()
  } else {
    x <- x |>
      dplyr::mutate(
        strata_name = stringr::str_to_sentence(gsub("&&&", "and", .data$strata_name)),
        strata_level = stringr::str_to_sentence(gsub("&&&", "and", .data$strata_level))
      )
  }

  # nice column names and values
  x <- x |>
    dplyr::mutate(
      cohort_name_reference = stringr::str_to_sentence(gsub("_", " ", .data$cohort_name_reference)),
      cohort_name_comparator = stringr::str_to_sentence(gsub("_", " ", .data$cohort_name_comparator)),
      variable_name = stringr::str_to_sentence(gsub("_", " ", .data$variable_name))
    ) |>
    dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", gsub("&&&", "and", .x)))) |>
    dplyr::rename("CDM name" = "Cdm name")

  if (type == "gt") {
    x <- x |>
      visOmopResults::gtTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  } else if (type == "flextable") {
    x <- x |>
      visOmopResults::fxTable(
        style = .options$style,
        na = .options$na,
        title = .options$title,
        subtitle = .options$subtitle,
        caption = .options$caption,
        groupNameCol = .options$groupNameCol,
        groupNameAsColumn = .options$groupNameAsColumn,
        groupOrder = .options$groupOrder,
        colsToMergeRows = .options$colsToMergeRows
      )
  }

  return(x)
}


formatOverlapEstimate <- function(count, percentage, .options) {
  paste0(
    niceNum(count, .options, "integer"),
    " (",
    niceNum(percentage, .options, "percentage"),
    "%)"
  )

}
niceNum <- function(num, .options, type) {
  trimws(format(round(num, .options$decimals[[type]]),
                big.mark = .options$bigMark,
                decimal.mark = .options$decimalMark,
                nsmall = .options$decimals[[type]],
                scientific = FALSE))
}

defaultOverlapOptions <- function(userOptions) {
  defaultOpts <- list(
    decimals = c(integer = 0, percentage = 1),
    decimalMark = ".",
    bigMark = ",",
    style = "default",
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameCol = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}

defaultColumnSelector <- function(input, column, column_name) {
  if (is.null(input)) {
    input <- unique(column)
  } else {
    notIn <- which(!input %in% unique(column))
    if (length(notIn) > 0) {
      cli::cli_warn("The following are not in {column_name} and will not be included:
                    {paste0(input[notIn], collapse = ', ')}")
    }
  }
  return(input)
}

#' Additional arguments for the function tableCohortOverlap.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCohortOverlap and their given default value.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableCohortOverlap()
#' }
#'
#'
optionsTableCohortOverlap <- function() {
  return(defaultOverlapOptions(NULL))
}

