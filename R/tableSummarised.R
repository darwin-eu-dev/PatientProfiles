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
tableCharacteristics <- function(result,
                                 type = "gt",
                                 formatEstimateName = c(
                                   "N (%)" = "<count> (<percentage>%)",
                                   "N" = "<count>",
                                   "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                   "Mean (SD)" = "<mean> (<sd>)",
                                   "Range" = "<min> to <max>"
                                 ),
                                 header = c("group"),
                                 split = c("group", "strata"),
                                 groupColumn = NULL,
                                 minCellCount = 5,
                                 excludeColumns = c("result_id", "result_type",
                                                    "package_name", "package_version",
                                                    "estimate_type", "additional_name",
                                                    "additional_level"),
                                 .options = list()) {

  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_characteristics", "summarised_demographics",
                      "summarised_cohort_insersect", "summarised_concept_insersect",
                      "summarised_table_insersect"))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # format table
  result <- formatTable(result = result,
                        formatEstimateName = formatEstimateName,
                        header = header,
                        groupColumn = groupColumn,
                        split = split,
                        type = type,
                        minCellCount = minCellCount,
                        excludeColumns = excludeColumns,
                        .options = .options)

  return(result)
}

defaultCharacteristicsOptions <- function(.options) {
  defaults <- list(
    "decimals" = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
    "decimalMark" = ".",
    "bigMark" = ",",
    "keepNotFormatted" = FALSE,
    "useFormatOrder" = TRUE,
    "delim" = "\n",
    "style" = "default",
    "na" = "-",
    "title" = NULL,
    "subtitle" = NULL,
    "caption" = NULL,
    "groupNameAsColumn" = FALSE,
    "groupOrder" = NULL,
    "colsToMergeRows" = "all_columns"
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
#' @param splitStrata If TRUE strata name-level columns will be split.
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
#'
tableCohortOverlap  <- function(result,
                                type = "gt",
                                formatEstimateName = c("N (%)" = "<count> (<percentage>%)"),
                                header = c("strata"),
                                split = c("group", "strata", "additional"),
                                groupColumn = NULL,
                                minCellCount = 5,
                                excludeColumns = c("result_id", "result_type",
                                                   "package_name", "package_version",
                                                   "estimate_type"),
                                .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertList(.options)

  # default
  .options <- defaultOverlapOptions(.options)

  # unique reference - comparator combinations
  if (.options$uniqueCombinations) {
    x <- result |>
      visOmopResults::splitGroup()
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference))) |>
      dplyr::mutate(variable_level = factor(.data$variable_level, levels = c("only_in_reference", "only_in_comparator", "overlap"))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(
        c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level", "variable_name", "variable_level")
      ))) |>
      dplyr::mutate(variable_level = as.character(variable_level)) |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    x <- result |>
      dplyr::mutate(variable_level = factor(.data$variable_level, levels = c("only_in_reference", "only_in_comparator", "overlap"))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(
        c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name", "variable_level")
      ))) |>
      dplyr::mutate(variable_level = as.character(variable_level))
  }

  # format table
  result <- formatTable(result = x,
                        formatEstimateName = formatEstimateName,
                        header = c(header, "variable"),
                        groupColumn = groupColumn,
                        split = split,
                        type = type,
                        minCellCount = minCellCount,
                        excludeColumns = excludeColumns,
                        .options = .options)

  return(result)
}

#' Format a cohort_timing object into a visual table.
#'
#' @param result A cohort_overlap object.
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param strataName Names of the strata names to include.
#' @param strataLevel Names of the strata levels to include.
#' @param splitStrata If TRUE strata name-level columns will be split.
#' @param cdmName Name of the databases to include.
#' @param variableName Name of the variable names to include.
#' @param formatEstimateName Whether to include the number of subjects.
#' @param header Vector with names and column names to use in the header of gt
#' and flextable.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", or "tibble".
#' @param minCellCount Counts below which results will be clouded.
#' @param .options named list with additional formatting options.
#' PatientProfiles::optionsTableCohortTiming() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
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
                              header = c("strata"),
                              split = c("group", "strata", "additional"),
                              groupColumn = NULL,
                              minCellCount = 5,
                              excludeColumns = c("result_id", "result_type",
                                                 "package_name", "package_version",
                                                 "estimate_type", "variable_level"),
                              .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertList(.options)

  # defaults
  .options <- defaultTimingOptions(.options)

  if (.options$uniqueCombinations) {
    x <- result |>
      visOmopResults::splitGroup()
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level")))) |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    x <- result |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c("cdm_name", "group_name", "group_level", "strata_name", "strata_level"))))
  }

  # format table
  result <- formatTable(result = x,
                        formatEstimateName = formatEstimateName,
                        header = header,
                        groupColumn = groupColumn,
                        split = split,
                        type = type,
                        minCellCount = minCellCount,
                        excludeColumns = excludeColumns,
                        .options = .options)

  return(result)
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
    uniqueCombinations = TRUE,
    c(integer = 0, percentage = 2, numeric = 2, proportion = 2),
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

defaultTimingOptions <- function(userOptions) {
  defaultOpts <- list(
    uniqueCombinations = TRUE,
    decimals = c(integer = 0, percentage = 2, numeric = 2, proportion = 2),
    decimalMark = ".",
    bigMark = ",",
    keepNotFormatted = TRUE,
    useFormatOrder = TRUE,
    delim = "\n",
    includeHeaderKey = TRUE,
    style = "default",
    na = "-",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    groupNameAsColumn = FALSE,
    groupOrder = NULL,
    colsToMergeRows = "all_columns"
  )

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}


# defaultColumnSelectors <- function(data, selectors) {
#   outSelectors <- list()
#   for (selector in names(selectors)) {
#     column <- data[[selector]]
#     input <- selectors[[selector]]
#     if (is.null(input)) {
#       input <- unique(column)
#     } else {
#       notIn <- which(!input %in% unique(column))
#       if (length(notIn) > 0) {
#         cli::cli_warn("The following are not in {selector} and will not be included:
#                     {paste0(input[notIn], collapse = ', ')}")
#       }
#     }
#     outSelectors[[selector]] <- input
#   }
#   return(outSelectors)
# }

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

#' Additional arguments for the function tableCohortTiming.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCohortTiming and their given default value.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableCohortTiming()
#' }
#'
#'
optionsTableCohortTiming <- function() {
  return(defaultTimingOptions(NULL))
}

