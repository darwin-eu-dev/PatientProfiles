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
#' @param .options .
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
    visOmopResults::splitGroup(overall = FALSE) |>
    visOmopResults::splitAdditional(overall = FALSE)
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

  colsStrata <- visOmopResults::strataColumns(result, overall = FALSE)
  if (length(colsStrata) > 0 & !splitStrata) {
    result <- result |>
      dplyr::mutate(
        "strata" = paste0(.data$strata_name, ": ", .data$strata_level)
      )
    colsStrata <- "strata"
  } else {
    result <- result |>
      visOmopResults::splitStrata(overall = FALSE)
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
  for (nm in names(defaults)) {
    if (!nm %in% names(.options)) {
      .options[[nm]] <- defaults[[nm]]
    }
  }
  return(.options)
}


#' Format a cohort_overlap object into a visual table.
#'
#' @param result A cohort_overlap object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", or "tibble".
#' @param numberSubjects Whether to include the number of subjects.
#' @param numberRecords Whether to include the number of records
#' @param cdmName Whether to include the CDM name.
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
                                cdmName = NULL,
                                type = "gt",
                                variableName = c("number records", "number subjects"),
                                minCellCount = 5,
                                .options = list()) {
  # initial checks
  # result <- omopgenerics::newSummarisedResult(result) |>
  #   dplyr::filter(.data$result_type == "cohort_overlap")
  # checkmate::assertChoice(type, c("gt", "flextable", "tibble"))
  # checkmate::assertLogical(numberSubjects, any.missing = FALSE, len = 1)
  # checkmate::assertLogical(numberRecords, any.missing = FALSE, len = 1)
  # checkmate::assertLogical(cdmName, any.missing = FALSE, len = 1)
  # checkmate::assertList(.options)

  # split table and supress
  x <- result |>
    visOmopResults::splitAll()

  # add default values
  .options <- defaultOverlapOptions(.options)
  if (is.null(cohortNameReference)) {
    cohortNameReference <- unique(x$cohort_name_reference)
  }
  if (is.null(cohortNameComparator)) {
    cohortNameComparator <- unique(x$cohort_name_comparator)
  }
  if (is.null(cdmName)) {
    cdmName <- unique(x$cdm_name)
  }

  x <- x |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$variable_name == .env$variableName) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    getTidyOverlap() |>
    dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    dplyr::arrange(dplyr::across(
      dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator")))) |>
    dplyr::mutate(
      total = .data$reference + .data$comparator,
      reference = .data$reference - .data$overlap,
      comparator = .data$comparator - .data$overlap
    ) |>
    dplyr::mutate(
      only_in_reference = dplyr::if_else(
        (.data$reference < .env$minCellCount) | is.na(.data$reference),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlap(.data$reference, .data$reference/.data$total * 100, .env$.options)
      ),
      only_in_comparator = dplyr::if_else(
        (.data$comparator < .env$minCellCount) | is.na(.data$comparator),
        paste0("<", niceNum(.env$minCellCount, .options, "integer")),
        formatOverlap(.data$comparator, .data$comparator/.data$total * 100, .env$.options)
      ),
      overlap = dplyr::if_else(
        (.data$overlap < .env$minCellCount) | is.na(.data$overlap),
        paste0("<",niceNum(.env$minCellCount, .options, "integer")),
        formatOverlap(.data$overlap, .data$overlap*2/.data$total * 100, .env$.options)),
      estimate_name = "N (%)"
    ) |>
    dplyr::select(c("cdm_name", "cohort_name_reference", "cohort_name_comparator",
                    "variable_name", "estimate_name", "only_in_reference",
                    "only_in_comparator", "overlap"))

  if (type == "gt") {
    x <- x |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
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
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
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
  } else if (type == "tibble") {
    x <- x |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x)))
  }

  return(x)
}

formatOverlap <- function(count, percentage, .options) {
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

getTidyOverlap <- function(x) {
  cohort_counts <- x |>
    dplyr::filter(.data$cohort_name_reference == .data$cohort_name_comparator)
  byCol <- colnames(x)
  x <- x |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
    dplyr::mutate(variable_level = "overlap") |>
    tidyr::pivot_wider(names_from = c("variable_level"),
                       values_from = "estimate_value") |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "reference") |>
        tidyr::pivot_wider(names_from = c("variable_level"),
                           values_from = "estimate_value") |>
        dplyr::select(!"cohort_name_comparator"),
      by = byCol[! byCol %in% c("cohort_name_comparator", "variable_level", "estimate_value")]
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "comparator") |>
        tidyr::pivot_wider(names_from = c("variable_level"),
                           values_from = "estimate_value") |>
        dplyr::select(!"cohort_name_reference"),
      by = byCol[! byCol %in% c("cohort_name_reference", "variable_level", "estimate_value")]
    )
  return(x)
}

#' Format a cohort_timing object into a visual table.
#'
#' @param result A cohort_timing object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", or "tibble".
#' @param formatEstimateName Whether to include the number of subjects.
#' @param header Whether to include the number of records
#' @param splitGroup description
#' @param splitStrata description
#' @param cdmName Whether to include the CDM name.
#' @param minCellCount Counts below which results will be clouded.
#' @param .options named list with additional formatting options.
#' PatientProfiles::tableCohortOverlapOptions() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' cdm_local <- omock::mockCdmReference() |>
#'   omock::mockPerson(100) |>
#'   omock::mockObservationPeriod() |>
#'   omock::mockCohort(numberCohorts = 2)
#'
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' cdm <- CDMConnector::copy_cdm_to(con = con,
#'                                  cdm = cdm_local,
#'                                  schema = "main"
#' cdm$cohort |>
#'   summariseCohortOverlap() |>
#'   tableCohortOverlap()
#' }
#'
#' @return A formatted table of the cohort_timing summarised object.
#'
#' @export
#'
tableCohortTiming <- function(result,
                              type = "gt",
                              formatEstimateName = c(
                                "N" = "<count>",
                                "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                "Range" = "<min> - <max>"
                              ),
                              header = character(),
                              splitGroup = TRUE,
                              splitStrata = TRUE,
                              cdmName = TRUE,
                              minCellCount = 5,
                              .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(type, c("gt", "flextable", "tibble"))
  checkmate::assertCharacter(formatEstimateName, any.missing = FALSE)
  checkmate::assertCharacter(header, any.missing = FALSE)
  checkmate::assertLogical(splitGroup, any.missing = FALSE, len = 1)
  checkmate::assertLogical(splitStrata, any.missing = FALSE, len = 1)
  checkmate::assertLogical(cdmName, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(minCellCount, any.missing = FALSE, len = 1)
  checkmate::assertList(.options)

  # options
  .options <- defaultTimingOptions(.options)

  result <- result |>
    omopgenerics::suppress(minCellCount = minCellCount) |>
    visOmopResults::formatEstimateValue(
      decimals = .options$decimals,
      decimalMark = .options$decimalMark,
      bigMark = .options$bigMark
    ) |>
    visOmopResults::formatEstimateName(
      estimateNameFormat = formatEstimateName,
      keepNotFormatted = .options$keepNotFormatted,
      useFormatOrder = .options$useFormatOrder
    ) |>
    visOmopResults::formatTable(
      header = header,
      delim = .options$delim,
      includeHeaderName = .options$includeHeaderName,
      includeHeaderKey = .options$includeHeaderKey
    ) |>
    dplyr::select(!c("result_type", "package_name", "package_version",
                     "additional_name", "additional_level", "estimate_type",
                     "variable_level"))
  if (splitGroup) {
    result <- result |>
      visOmopResults::splitGroup()
  }
  if (splitStrata) {
    result <- result |>
      visOmopResults::splitStrata()
  }

  if (!cdmName) {
    result <- result |>
      dplyr::select(!"cdm_name")
  } else {
    result <- result |>
      dplyr::rename("Database name" = "cdm_name")
  }

  if (type == "gt") {
    result <- result |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
      visOmopResults::gtTable(
        delim = .options$delim,
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
    result <- result |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x))) |>
      visOmopResults::fxTable(
        delim = .options$delim,
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
  } else if (type == "tibble") {
    result <- result |>
      dplyr::rename_with(~stringr::str_to_sentence(gsub("_", " ", .x)))
  }
  return(result)
}


defaultTimingOptions <- function(userOptions) {
  defaultOpts <- list(
    decimals = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    decimalMark = ".",
    bigMark = ",",
    keepNotFormatted = TRUE,
    useFormatOrder = TRUE,
    includeHeaderName = TRUE,
    includeHeaderKey = TRUE,
    delim = "\n",
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
