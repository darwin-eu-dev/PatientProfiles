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
#' @param x Tibble with the individuals that we want to add the age.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param ageAt Variable that points the date to compute the age. By default:
#' 'cohort_start_date'
#' @param defaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param defaultDay day of the month assigned to individuals with missing day
#' of birth. By default: 1.
#' @param imposeMonth Weather the month of the date of birth will be considered
#' as missing for all the individuals. By default: TRUE.
#' @param imposeDay Weather the day of the date of birth will be considered as
#' missing for all the individuals. By default: TRUE.
#' @param compute Weather resultant table will be computed as temporal table. By
#' default: TRUE.
#'
#' @return
#' @export
#'
#' @examples

addAge <- function(x,
                   cdm,
                   ageAt = "cohort_start_date",
                   defaultMonth = 1,
                   defaultDay = 1,
                   imposeMonth = TRUE,
                   imposeDay = TRUE,
                   compute = TRUE) {
  defaultMonth <- as.integer(defaultMonth)
  defaultDay <- as.integer(defaultDay)

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

#' @noRd
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
