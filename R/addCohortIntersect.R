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

#' Compute the intersect with a target cohort, you can compute the number of
#' occurrences, a flag of presence, a certain date and/or the time difference
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#' @param order which record is considered in case of multiple records
#' @param flag TRUE or FALSE. If TRUE, flag will calculated for this
#' intersection
#' @param count TRUE or FALSE. If TRUE, the number of counts will be calculated
#' for this intersection
#' @param date TRUE or FALSE. If TRUE, date will be calculated for this
#' intersection
#' @param days TRUE or FALSE. If TRUE, time difference in days will be
#' calculated for this intersection
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with overlap information
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addCohortIntersect(
#'     targetCohortTable = "cohort2"
#'   )
#' }
#'
addCohortIntersect <- function(x,
                               cdm = lifecycle::deprecated(),
                               targetCohortTable,
                               targetCohortId = NULL,
                               indexDate = "cohort_start_date",
                               censorDate = NULL,
                               targetStartDate = "cohort_start_date",
                               targetEndDate = "cohort_end_date",
                               window = list(c(0, Inf)),
                               order = "first",
                               flag = TRUE,
                               count = TRUE,
                               date = TRUE,
                               days = TRUE,
                               nameStyle = "{value}_{cohort_name}_{window_name}") {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "addCohortIntersect()",
    details = c(
      "please use the specific functions instead:",
      "*" = "addCohortIntersectFlag()", "*" = "addCohortIntersectCount()",
      "*" = "addCohortIntersectDate()", "*" = "addCohortIntersectDays()"
    )
  )
  .addCohortIntersect(
    x = x, targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId, indexDate = indexDate,
    censorDate = censorDate, targetStartDate = targetStartDate,
    targetEndDate = targetEndDate, window = window, order = order, flag = flag,
    count = count, date = date, days = days, nameStyle = nameStyle
  )
}

.addCohortIntersect <- function(x,
                                targetCohortTable,
                                targetCohortId = NULL,
                                indexDate = "cohort_start_date",
                                censorDate = NULL,
                                targetStartDate = "cohort_start_date",
                                targetEndDate = "cohort_end_date",
                                window = list(c(0, Inf)),
                                order = "first",
                                flag = TRUE,
                                count = TRUE,
                                date = TRUE,
                                days = TRUE,
                                nameStyle = "{value}_{cohort_name}_{window_name}") {
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)
  checkmate::assertLogical(flag, any.missing = FALSE, len = 1)
  checkmate::assertLogical(count, any.missing = FALSE, len = 1)
  checkmate::assertLogical(date, any.missing = FALSE, len = 1)
  checkmate::assertLogical(days, any.missing = FALSE, len = 1)
  checkmate::assertTRUE(flag | count | date | days)
  value <- c("flag", "count", "date", "days")[c(flag, count, date, days)]

  x <- x %>%
    addIntersect(
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = value,
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' It creates columns to indicate the presence of cohorts
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with overlap information
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addCohortIntersectFlag(
#'     targetCohortTable = "cohort2"
#'   )
#' }
#'
addCohortIntersectFlag <- function(x,
                                   cdm = lifecycle::deprecated(),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetStartDate = "cohort_start_date",
                                   targetEndDate = "cohort_end_date",
                                   window = list(c(0, Inf)),
                                   nameStyle = "{cohort_name}_{window_name}") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addCohortIntersectFlag(cdm)")
  }
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = "flag",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' It creates columns to indicate number of occurrences of intersection with a
#' cohort
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param targetCohortTable name of the cohort that we want to check for overlap
#' @param targetCohortId vector of cohort definition ids to include
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param window window to consider events of
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with overlap information
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addCohortIntersectCount(
#'     targetCohortTable = "cohort2"
#'   )
#' }
#'
addCohortIntersectCount <- function(x,
                                    cdm = lifecycle::deprecated(),
                                    targetCohortTable,
                                    targetCohortId = NULL,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    targetStartDate = "cohort_start_date",
                                    targetEndDate = "cohort_end_date",
                                    window = list(c(0, Inf)),
                                    nameStyle = "{cohort_name}_{window_name}") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addCohortIntersectCount(cdm)")
  }
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      tableName = targetCohortTable,
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      value = "count",
      indexDate = indexDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      window = window,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}

#' It creates columns to indicate the number of days between the current table
#' and a target cohort
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param targetCohortTable Cohort table to
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a days variable added for each
#' cohort of interest
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addCohortIntersectDays(
#'     targetCohortTable = "cohort2"
#'   )
#' }
#'
addCohortIntersectDays <- function(x,
                                   cdm = lifecycle::deprecated(),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addCohortIntersectDays(cdm)")
  }
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "days",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}


#' Date of cohorts that are present in a certain window
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param targetCohortTable Cohort table to
#' @param targetCohortId Cohort IDs of interest from the other cohort table. If
#' NULL, all cohorts will be used with a time variable added for each
#' cohort of interest
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a specific date
#' or a column date of x
#' @param targetDate Date of interest in the other cohort table. Either
#' cohort_start_date or cohort_end_date
#' @param order date to use if there are multiple records for an
#' individual during the window of interest. Either first or last.
#' @param window Window of time to identify records relative to the indexDate.
#' Records outside of this time period will be ignored.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return x along with additional columns for each cohort of interest.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addCohortIntersectDate(
#'     targetCohortTable = "cohort2"
#'   )
#' }
#'
addCohortIntersectDate <- function(x,
                                   cdm = lifecycle::deprecated(),
                                   targetCohortTable,
                                   targetCohortId = NULL,
                                   indexDate = "cohort_start_date",
                                   censorDate = NULL,
                                   targetDate = "cohort_start_date",
                                   order = "first",
                                   window = c(0, Inf),
                                   nameStyle = "{cohort_name}_{window_name}") {
  if (lifecycle::is_present(cdm)) {
    lifecycle::deprecate_warn("0.6.0", "addCohortIntersectDate(cdm)")
  }
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = targetCohortTable)
  checkmate::assertNumeric(targetCohortId, any.missing = FALSE, null.ok = TRUE)
  parameters <- checkCohortNames(cdm[[targetCohortTable]], targetCohortId, targetCohortTable)
  nameStyle <- gsub("\\{cohort_name\\}", "\\{id_name\\}", nameStyle)

  x <- x %>%
    addIntersect(
      tableName = targetCohortTable,
      indexDate = indexDate,
      value = "date",
      filterVariable = parameters$filter_variable,
      filterId = parameters$filter_id,
      idName = parameters$id_name,
      window = window,
      targetStartDate = targetDate,
      targetEndDate = NULL,
      order = order,
      nameStyle = nameStyle,
      censorDate = censorDate
    )

  return(x)
}
