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

#' Compute the intersect with an omop table, you can compute the number of
#' occurrences, a flag of presence, a certain date, the time difference and/or
#' obtain a certain column.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param overlap Whether to consider end date or only end date for the
#' insersection.
#' @param flag TRUE or FALSE. If TRUE, flag will calculated for this
#' intersection
#' @param count TRUE or FALSE. If TRUE, the number of counts will be calculated
#' for this intersection
#' @param date TRUE or FALSE. If TRUE, date will be calculated for this
#' intersection
#' @param days TRUE or FALSE. If TRUE, time difference in days will be
#' calculated for this intersection
#' @param extraValue Other columns from the table to intersect.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersect(tableName = "visit_occurrence")
#' }
#'
addTableIntersect <- function(x,
                              tableName,
                              indexDate = "cohort_start_date",
                              censorDate = NULL,
                              window = list(c(0, Inf)),
                              order = "first",
                              overlap = TRUE,
                              flag = TRUE,
                              count = TRUE,
                              date = TRUE,
                              days = TRUE,
                              extraValue = character(),
                              nameStyle = "{table_name}_{value}_{window_name}") {
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", "tableName", nameStyle)
  checkmate::assertLogical(flag, any.missing = FALSE, len = 1)
  checkmate::assertLogical(count, any.missing = FALSE, len = 1)
  checkmate::assertLogical(date, any.missing = FALSE, len = 1)
  checkmate::assertLogical(days, any.missing = FALSE, len = 1)
  checkmate::assertLogical(overlap, any.missing = FALSE, len = 1)
  checkmate::assertTRUE(flag | count | date | days | length(extraValue)>0)
  value <- c("flag", "count", "date", "days")[c(flag, count, date, days)]
  value <- c(value, extraValue)

  targetEndDate <- ifelse(
    overlap, endDateColumn(tableName), startDateColumn(tableName)
  )

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = value,
      indexDate = indexDate,
      targetStartDate = startDateColumn(tableName),
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' Compute a flag intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param overlap Whether to consider end date or only end date for the
#' insersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectFlag(tableName = "visit_occurrence")
#' }
#'
addTableIntersectFlag <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  order = "first",
                                  overlap = TRUE,
                                  nameStyle = "{table_name}_{window_name}") {
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", "tableName", nameStyle)
  checkmate::assertLogical(overlap, any.missing = FALSE, len = 1)

  targetEndDate <- ifelse(
    overlap, endDateColumn(tableName), startDateColumn(tableName)
  )

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = startDateColumn(tableName),
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' Compute number of intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param overlap Whether to consider end date or only end date for the
#' insersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectCount(tableName = "visit_occurrence")
#' }
#'
addTableIntersectCount <- function(x,
                                   tableName,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   window = list(c(0, Inf)),
                                   order = "first",
                                   overlap = TRUE,
                                   nameStyle = "{table_name}_{window_name}") {
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", "tableName", nameStyle)
  checkmate::assertLogical(overlap, any.missing = FALSE, len = 1)

  targetEndDate <- ifelse(
    overlap, endDateColumn(tableName), startDateColumn(tableName)
  )

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "count",
      indexDate = indexDate,
      targetStartDate = startDateColumn(tableName),
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' Compute date of intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param overlap Whether to consider end date or only end date for the
#' insersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectDate(tableName = "visit_occurrence")
#' }
#'
addTableIntersectDate <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  order = "first",
                                  overlap = TRUE,
                                  nameStyle = "{table_name}_{window_name}") {
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", "tableName", nameStyle)
  checkmate::assertLogical(overlap, any.missing = FALSE, len = 1)

  targetEndDate <- ifelse(
    overlap, endDateColumn(tableName), startDateColumn(tableName)
  )

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "date",
      indexDate = indexDate,
      targetStartDate = startDateColumn(tableName),
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' Compute time to intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectDays(tableName = "visit_occurrence")
#' }
#'
addTableIntersectDays <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  order = "first",
                                  overlap = TRUE,
                                  nameStyle = "{table_name}_{window_name}") {
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", "tableName", nameStyle)
  checkmate::assertLogical(overlap, any.missing = FALSE, len = 1)

  targetEndDate <- ifelse(
    overlap, endDateColumn(tableName), startDateColumn(tableName)
  )

  x <- x %>%
    addIntersect(
      cdm = cdm,
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "days",
      indexDate = indexDate,
      targetStartDate = startDateColumn(tableName),
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}
