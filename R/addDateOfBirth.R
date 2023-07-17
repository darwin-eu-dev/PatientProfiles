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

#' Add a column with the individual birth date
#'
#' @param x Table in the cdm that contains 'person_id' or 'subject_id'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param name Name of the column to be added with the date of birth
#' @param missingDay Day of the individuals with no or imposed day of birth
#' @param missingMonth Month of the individuals with no or imposed month of
#' birth
#' @param imposeDay Whether to impose day of birth
#' @param imposeMonth Whether to impose month of birth
#'
#' @return The function returns the table x with an extra column that contains
#' the date of birth
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDateOfBirth()
#' }
addDateOfBirth <- function(x,
                           cdm = attr(x, "cdm_reference"),
                           name = "date_of_birth",
                           missingDay = 1,
                           missingMonth = 1,
                           imposeDay = FALSE,
                           imposeMonth = FALSE) {
  # initial checks
  # parameters <- checkInputs(
  #   x, cdm, name, misisngDay, missingMonth, imposeDay, imposeMonth
  # )
  # get parameters
  # personIdentifier <- parameters$person_identifier
  # impose day

  personVariable <- checkX(x)

  cdm$person <- cdm$person %>%
    dplyr::filter(!is.na(.data$year_of_birth))

  # First add to person table
  if (imposeDay) {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = missingDay)
  } else {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth), .env$missingDay, .data$day_of_birth
      ))
  }
  # impose month
  if (imposeMonth) {
    person <- person %>%
      dplyr::mutate(month_of_birth = missingMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth), .env$missingMonth, .data$month_of_birth
      ))
  }
  person <- person %>%
    dplyr::mutate(
      year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
      month_of_birth1 = as.character(as.integer(.data$month_of_birth)),
      day_of_birth1 = as.character(as.integer(.data$day_of_birth))
    ) %>%
    dplyr::mutate(!!name := as.Date(paste0(
      .data$year_of_birth1, "-", .data$month_of_birth1, "-",
      .data$day_of_birth1
    ))) %>%
    dplyr::select(!c("year_of_birth1", "month_of_birth1", "day_of_birth1"))


  # Add to other table (if not person)
  if (isPersonTable(x)) {
    return(person)
  } else {
    x <- x %>%
      dplyr::left_join(
        person %>%
          dplyr::rename(!!personVariable := "person_id") %>%
          dplyr::select(dplyr::all_of(c(personVariable, .env$name))),
        by = personVariable
      )
    return(x)
  }
}

isPersonTable <- function(x) {
  return(all(colnames(x) %in%
    c(
      "person_id", "gender_concept_id",
      "year_of_birth", "month_of_birth", "day_of_birth"
    )))
}
