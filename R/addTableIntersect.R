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

#' Compute a flag intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen,
#' episode.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param targetStartDate Column name with start date for comparison.
#' @param targetEndDate Column name with end date for comparison.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table with added columns with intersect information.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectFlag(tableName = "visit_occurrence")
#' mockDisconnect(cdm = cdm)
#' }
#'
addTableIntersectFlag <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  targetStartDate = startDateColumn(tableName),
                                  targetEndDate = endDateColumn(tableName),
                                  nameStyle = "{table_name}_{window_name}",
                                  name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  assertCharacter(tableName)
  checkCdm(cdm, tables = tableName)
  assertCharacter(tableName)
  nameStyle <- gsub("\\{table_name\\}", tableName, nameStyle)

  x <- x %>%
    .addIntersect(
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      order = "first",
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' Compute number of intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen,
#' episode.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param targetStartDate Column name with start date for comparison.
#' @param targetEndDate Column name with end date for comparison.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table with added columns with intersect information.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addTableIntersectCount(tableName = "visit_occurrence")
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addTableIntersectCount <- function(x,
                                   tableName,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   window = list(c(0, Inf)),
                                   targetStartDate = startDateColumn(tableName),
                                   targetEndDate = endDateColumn(tableName),
                                   nameStyle = "{table_name}_{window_name}",
                                   name = NULL) {


  cdm <- omopgenerics::cdmReference(x)
  assertCharacter(tableName)
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", tableName, nameStyle)

  x <- x %>%
    .addIntersect(
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "count",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      order = "first",
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' Compute date of intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen,
#' episode.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param targetDate Target date in tableName.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
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
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addTableIntersectDate <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  targetDate = startDateColumn(tableName),
                                  order = "first",
                                  nameStyle = "{table_name}_{window_name}",
                                  name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  assertCharacter(tableName)
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", tableName, nameStyle)

  x <- x %>%
    .addIntersect(
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "date",
      indexDate = indexDate,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' Compute time to intersect with an omop table.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen,
#' episode.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in.
#' @param targetDate Target date in tableName.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
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
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
addTableIntersectDays <- function(x,
                                  tableName,
                                  indexDate = "cohort_start_date",
                                  censorDate = NULL,
                                  window = list(c(0, Inf)),
                                  targetDate = startDateColumn(tableName),
                                  order = "first",
                                  nameStyle = "{table_name}_{window_name}",
                                  name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  assertCharacter(tableName)
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", tableName, nameStyle)

  x <- x %>%
    .addIntersect(
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = "days",
      indexDate = indexDate,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}

#' Intersecting the cohort with columns of an OMOP table of user's choice.
#' It will add an extra column to the cohort, indicating the intersected
#' entries with the target columns in a window of the user's choice.
#'
#' @param x Table with individuals in the cdm.
#' @param tableName Name of the table to intersect with. Options:
#' visit_occurrence, condition_occurrence, drug_exposure, procedure_occurrence,
#' device_exposure, measurement, observation, drug_era, condition_era, specimen,
#' episode.
#' @param field The columns from the table in tableName to intersect over.
#' For example, if the user uses visit_occurrence in tableName then for field the possible
#' options include visit_occurrence_id, visit_concept_id, visit_type_concept_id.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x.
#' @param window window to consider events in when intersecting with the chosen column.
#' @param targetDate The dates in the target columns in tableName that the user may want to restrict to.
#' @param order which record is considered in case of multiple records (only
#' required for date and days options).
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table with added columns with intersect information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addTableIntersectField(
#'     tableName = "visit_occurrence",
#'     field = "visit_concept_id",
#'     order = "last",
#'     window = c(-Inf, -1)
#'   )
#' mockDisconnect(cdm = cdm)
#' }
#'
addTableIntersectField <- function(x,
                                   tableName,
                                   field,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   window = list(c(0, Inf)),
                                   targetDate = startDateColumn(tableName),
                                   order = "first",
                                   nameStyle = "{table_name}_{extra_value}_{window_name}",
                                   name = NULL) {
  cdm <- omopgenerics::cdmReference(x)
  assertCharacter(tableName)
  checkCdm(cdm, tables = tableName)
  nameStyle <- gsub("\\{table_name\\}", tableName, nameStyle)
  nameStyle <- gsub("\\{extra_value\\}", "\\{value\\}", nameStyle)

  x <- x %>%
    .addIntersect(
      tableName = tableName,
      filterVariable = NULL,
      filterId = NULL,
      idName = NULL,
      value = field,
      indexDate = indexDate,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate,
      name = name
    )

  return(x)
}
