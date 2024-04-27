# Copyright 2024 DARWIN EU (C)
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
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summarised_characteristics object.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' PatientProfiles::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   tableCharacteristics()
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of the summariseCharacteristics
#' result.
#'
#' @export
#'
tableCharacteristics <- function(result,
                                 type = "gt",
                                 formatEstimateName = c(
                                   "N (%)" = "<count> (<percentage>%)",
                                   "N" = "<count>",
                                   "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                   "[Q05 - Q95]" = "[<q05> - <q95>]",
                                   "Mean (SD)" = "<mean> (<sd>)",
                                   "Range" = "<min> to <max>"
                                 ),
                                 header = c("group"),
                                 split = c("group", "strata"),
                                 groupColumn = NULL,
                                 minCellCount = 5,
                                 excludeColumns = c("result_id", "estimate_type",
                                                    "additional_name", "additional_level"),
                                 .options = list()) {

  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in%
                    c("summarised_characteristics", "summarised_demographics",
                      "summarised_cohort_intersect", "summarised_concept_intersect",
                      "summarised_table_intersect"))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # format table
  result <- visOmopResults::formatTable(
    result = result,
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
    "keepNotFormatted" = TRUE,
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

#' Additional arguments for the function tableCharacteristics.
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableCharacteristics, and their given default values.
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
optionsTableCharacteristics <- function() {
  return(defaultCharacteristicsOptions(NULL))
}

#' Format a summariseOverlapCohort result into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summariseOverlapCohort result.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' PatientProfiles::optionsTableCohortOverlap() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' tableCohortOverlap(overlap)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A formatted table of the summariseOverlapCohort result.
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
                                excludeColumns = c("result_id", "estimate_type"),
                                .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "cohort_overlap")
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
      dplyr::mutate(variable_level = as.character(.data$variable_level)) |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
  } else {
    x <- result |>
      dplyr::mutate(variable_level = factor(.data$variable_level, levels = c("only_in_reference", "only_in_comparator", "overlap"))) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(
        c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name", "variable_level")
      ))) |>
      dplyr::mutate(variable_level = as.character(.data$variable_level))
  }

  # format table
  result <- visOmopResults::formatTable(result = x,
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
optionsTableCohortOverlap <- function() {
  return(defaultOverlapOptions(NULL))
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

#' Format a summariseCohortTiming result into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summariseCohortTiming result
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options named list with additional formatting options.
#' PatientProfiles::optionsTableCohortTiming() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' timing <- summariseCohortTiming(cdm$cohort2)
#' tableCohortTiming(timing)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A formatted table of the summariseCohortTiming result.
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
                              excludeColumns = c("result_id", "estimate_type",
                                                 "variable_level"),
                              .options = list()) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type == "cohort_timing")
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
  result <- visOmopResults::formatTable(result = x,
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

#' Format a summariseTableIntersect result into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A result from summariseTableIntersect.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' PatientProfiles::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#'
#'
#' @return A table with a formatted version of a summariseTableIntersect
#' result.
#'
tableTableIntersect <- function(result,
                                 type = "gt",
                                 formatEstimateName = c(
                                   "N (%)" = "<count> (<percentage>%)",
                                   "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                   "Mean (SD)" = "<mean> (<sd>)",
                                   "Range" = "<min> to <max>"
                                 ),
                                 header = c("group"),
                                 split = c("group", "strata"),
                                 groupColumn = NULL,
                                 minCellCount = 5,
                                 excludeColumns = c("result_id", "estimate_type",
                                                    "variable_level", "additional_name",
                                                    "additional_level"),
                                 .options = list()) {

  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in%
                    c("summarised_table_intersect"))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # format table
  result <- visOmopResults::formatTable(
    result = result,
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


#' Format a summariseCohortIntersect result into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A result from summariseCohortIntersect.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' PatientProfiles::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'  summariseCohortIntersect(
#'   cohortIntersect = list(
#'     "Medications in the prior year" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'     )
#'    )
#'   ) |>
#'   tableCohortIntersect()
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of a summariseCohortIntersect
#' result.
#'
#' @export
#'
tableCohortIntersect <- function(result,
                                type = "gt",
                                formatEstimateName = c(
                                  "N (%)" = "<count> (<percentage>%)",
                                  "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                  "Mean (SD)" = "<mean> (<sd>)",
                                  "Range" = "<min> to <max>"
                                ),
                                header = c("group"),
                                split = c("group", "strata"),
                                groupColumn = NULL,
                                minCellCount = 5,
                                excludeColumns = c("result_id", "estimate_type",
                                                   "additional_name", "additional_level"),
                                .options = list()) {

  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in%
                    c("summarised_cohort_intersect"))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # format table
  result <- visOmopResults::formatTable(
    result = result,
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

#' Format a summarised_large_scale_characteristics object into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summarised_large_scalecharacteristics object.
#' @param type Output type ("gt" or "flextable").
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param splitStrata Whether to split strata_group and strata_level to multiple
#' columns.
#' @param header Specify the headers of the table.
#' @param topConcepts Number of concepts to restrict the table.
#' @param minCellCount Minimum number of counts to display.
#'
#' @export
#'
#' @return A formatted table.
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(CDMConnector)
#'
#' con <- dbConnect(duckdb(), eunomia_dir())
#' cdm <- cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")
#' cdm <- generateConceptCohortSet(
#'   cdm = cdm,
#'   conceptSet = list("viral_pharyngitis" = 4112343),
#'   name = "my_cohort"
#' )
#' result <- summariseLargeScaleCharacteristics(
#'   cohort = cdm$my_cohort,
#'   eventInWindow = "condition_occurrence",
#'   episodeInWindow = "drug_exposure"
#' )
#' tableLargeScaleCharacteristics(result)
#' }
#'
tableLargeScaleCharacteristics <- function(result,
                                           type = "gt",
                                           formatEstimateName = c("N (%)" = "<count> (<percentage>%)"),
                                           splitStrata = TRUE,
                                           header = c("cdm name", "cohort name", "strata", "window name"),
                                           topConcepts = 10,
                                           minCellCount = 5) {
  assertClass(result, "summarised_result")
  assertLogical(splitStrata, length = 1)
  if (is.character(header)) {
    header <- tolower(header)
    header <- gsub("_", " ", header)
  }
  assertChoice(header, choices = c("cdm name", "cohort name", "strata", "window name"))
  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "summarised_large_scale_characteristics")
  if (nrow(result) == 0) {
    cli::cli_abort(
      "No summarised_large_scale_characteristics records where found in this result object"
    )
  }
  sets <- result |>
    dplyr::filter(.data$variable_name == "settings") |>
    dplyr::select("result_id", "estimate_name", "estimate_value") |>
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
    dplyr::mutate("group" = paste0(
      "Table: ", .data$table_name, "; Type: ", .data$type, "; Analysis: ",
      .data$analysis
    )) |>
    dplyr::select("result_id", "group")
  res <- result |>
    dplyr::filter(.data$variable_name != "settings") |>
    omopgenerics::suppress(minCellCount = minCellCount) |>
    visOmopResults::splitGroup() |>
    visOmopResults::splitAdditional() |>
    dplyr::inner_join(sets, by = "result_id") |>
    dplyr::rename("window_name" = "variable_level") |>
    dplyr::select(!c("package_name", "package_version", "result_id", "result_type"))
  if (splitStrata) {
    res <- res |> visOmopResults::splitStrata()
    strataColumns <- visOmopResults::strataColumns(result)
  } else {
    strataColumns <- c("strata_name", "strata_level")
  }
  # get only topN
  top <- res |>
    dplyr::filter(.data$estimate_name == "count") |>
    dplyr::select("concept_id", "estimate_value", "group") |>
    dplyr::mutate("estimate_value" = as.numeric(.data$estimate_value)) |>
    dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
    dplyr::select("concept_id", "group") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate("order_id" = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$order_id <= .env$topConcepts) |>
    dplyr::select(-"group")
  res <- res |>
    dplyr::inner_join(top, by = "concept_id") |>
    visOmopResults::formatEstimateValue() |>
    visOmopResults::formatEstimateName(estimateNameFormat = formatEstimateName) |>
    orderWindow() |>
    dplyr::arrange(!!!rlang::syms(c(
      "cdm_name", "cohort_name", strataColumns, "group", "window_id",
      "order_id"
    ))) |>
    dplyr::mutate(
      "Concept" = paste0(.data$variable_name, " (", .data$concept_id, ")")
    ) |>
    dplyr::select(dplyr::all_of(c(
      "group", "CDM name" = "cdm_name", "Cohort name" =  "cohort_name",
      strataColumns, "Concept", "Window" = "window_name", "estimate_value"
    )))

  header <- cleanHeader(header, strataColumns)
  tab <- visOmopResults::formatHeader(result = res, header = header)
  if (type == "gt") {
    res <- visOmopResults::gtTable(tab, groupNameCol = "group")
  } else {
    res <- visOmopResults::fxTable(tab, groupNameCol = "group")
  }

  return(res)
}
cleanHeader <- function(header, strata) {
  header[header == "cdm name"] <- "CDM name"
  header[header == "cohort name"] <- "Cohort name"
  header[header == "window name"] <- "Window"
  if ("strata" %in% header) {
    id <- which(header == "strata")
    header <- append(header, strata, after=id)
    header <- header[header != "strata"]
  }
}
orderWindow <- function(res) {
  windows <- res |>
    dplyr::select("window_name") |>
    dplyr::distinct() |>
    dplyr::pull()
  win <- windows |>
    stringr::str_split(pattern = " ") |>
    lapply(function(x) {
      if (length(x) == 3) {
        if (x[2] == "to") {
          return(dplyr::tibble(
            lower = as.numeric(x[1]), upper = as.numeric(x[3])
          ))
        }
      }
      return(dplyr::tibble(lower = NA, upper = NA))
    })
  names(win) <- windows
  tib <- dplyr::bind_rows(win, .id = "window_name") |>
    dplyr::arrange(.data$lower, .data$upper) |>
    dplyr::mutate("window_id" = dplyr::row_number())
  res <- res |>
    dplyr::left_join(tib, by = "window_name")
  return(res)
}

#' Format a summariseDemographics result into a visual table.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A result from summariseDemographics.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param formatEstimateName Named list of estimate name's to join, sorted by
#' computation order. Indicate estimate_name's between <...>.
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param split A vector containing the name-level groups to split ("group",
#' "strata", "additional"), or an empty character vector to not split.
#' @param groupColumn Column to use as group labels.
#' @param minCellCount Counts below which results will be clouded.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' PatientProfiles::optionsTableCharacteristics() shows allowed arguments and
#' their default values.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'  summariseDemographics() |>
#'  tableDemographics()
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
#' @return A table with a formatted version of a summariseDemographics result.
#'
#' @export
#'
tableDemographics <- function(result,
                                 type = "gt",
                                 formatEstimateName = c(
                                   "N (%)" = "<count> (<percentage>%)",
                                   "Median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
                                   "[Q05 - Q95]" = "[<q05> - <q95>]",
                                   "Mean (SD)" = "<mean> (<sd>)",
                                   "Range" = "<min> to <max>"
                                 ),
                                 header = c("group"),
                                 split = c("group", "strata"),
                                 groupColumn = NULL,
                                 minCellCount = 5,
                                 excludeColumns = c("result_id", "estimate_type",
                                                    "additional_name", "additional_level"),
                                 .options = list()) {

  # check input
  result <- omopgenerics::newSummarisedResult(result) |>
    visOmopResults::filterSettings(.data$result_type %in%
                    c("summarised_demographics"))
  checkmate::assertList(.options)

  # add default options
  .options <- defaultCharacteristicsOptions(.options)

  # format table
  result <- visOmopResults::formatTable(
    result = result,
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
