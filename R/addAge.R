# Copyright 2022 DARWIN EU (C)
#
# This file is part of CohortProfiles
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

#' Add a column to the current tibble with the age of the subject_id at a
#' certain date
#'
#' @param x Tibble with the individuals that we want to add the age. Need to be in cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param ageAt Variable that points the date to compute the age. By default:
#' 'cohort_start_date'
#' @param defaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param defaultDay day of the month assigned to individuals with missing day
#' of birth. By default: 1.
#' @param imposeMonth Whether the month of the date of birth will be considered
#' as missing for all the individuals. By default: TRUE.
#' @param imposeDay Whether the day of the date of birth will be considered as
#' missing for all the individuals. By default: TRUE.
#' @param compute Whether resultant table will be computed as temporal table. By
#' default: TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(CohortProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-01-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
#'   )
#' )
#'
#' person <- tibble::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8532", "8507"),
#'   year_of_birth = c(2000, 1995, NA),
#'   month_of_birth = c(NA, 07, 08),
#'   day_of_birth = c(01, 25, 03)
#' )
#' cdm <- mockDrugUtilisation(person = person, cohort1 = cohort1)
#' addAge(x = cdm[["cohort1"]], cdm = cdm)
#' }
addAge <- function(x,
                   cdm,
                   ageAt = "cohort_start_date",
                   defaultMonth = 1,
                   defaultDay = 1,
                   imposeMonth = TRUE,
                   imposeDay = TRUE,
                   compute = TRUE) {
  messageStore <- checkmate::makeAssertCollection()

  checkmate::assertClass(cdm, "cdm_reference", add = messageStore)

  # check if ageAt length = 1 and is in table x
  checkmate::assertCharacter(ageAt, len = 1, add = messageStore)

  ageAtExists <- checkmate::assertTRUE(ageAt %in% colnames(x), add = messageStore)

  subjectExists <- checkmate::assertTRUE("subject_id" %in% colnames(x), add = messageStore)

  if (!isTRUE(ageAtExists)) {
    messageStore$push("- ageAt not found in table")
  }

  if (!isTRUE(subjectExists)) {
    messageStore$push("- subject_id not found in table")
  }


  # check if default imputation value for month and day are within range allowed
  checkmate::assertInt(defaultMonth, lower = 1, upper = 12)
  checkmate::assertInt(defaultDay, lower = 1, upper = 31)

  # check if imposeMonth imposeDay and compute are logical
  checkmate::assertLogical(imposeMonth, add = messageStore)
  checkmate::assertLogical(imposeDay, add = messageStore)
  checkmate::assertLogical(compute, add = messageStore)

  defaultMonth <- as.integer(defaultMonth)
  defaultDay <- as.integer(defaultDay)

  checkmate::reportAssertions(collection = messageStore)

  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", dplyr::all_of(ageAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    )

  if (imposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$defaultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$defaultMonth,
        .data$month_of_birth
      ))
  }

  if (imposeDay == TRUE) {
    person <- person %>%
      dplyr::mutate(day_of_birth = .env$defaultDay)
  } else {
    person <- person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth),
        .env$defaultDay,
        .data$day_of_birth
      ))
  }

  person <- person %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(year_of_birth1 = as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1 = as.character(as.integer(.data$month_of_birth))) %>%
    dplyr::mutate(day_of_birth1 = as.character(as.integer(.data$day_of_birth))) %>%
    dplyr::mutate(birth_date = as.Date(paste0(
      .data$year_of_birth1, "-",
      .data$month_of_birth1, "-",
      .data$day_of_birth1
    ))) %>%
    dplyr::mutate(age = floor(dbplyr::sql(sqlGetAge(
      dialect = CDMConnector::dbms(cdm),
      dob = "birth_date",
      dateOfInterest = ageAt
    )))) %>%
    dplyr::select("subject_id", dplyr::all_of(ageAt), "age") %>%
    dplyr::right_join(x, by = c("subject_id", ageAt)) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "age")
  if (isTRUE(compute)) {
    person <- person %>% dplyr::compute()
  }
  return(person)
}


sqlGetAge <- function(dialect,
                      dob,
                      dateOfInterest) {
  SqlRender::translate(
    SqlRender::render("((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
      dob = dob,
      date_of_interest = dateOfInterest
    ),
    targetDialect = dialect
  )
}
