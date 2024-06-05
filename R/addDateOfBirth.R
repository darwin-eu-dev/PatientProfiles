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

#' Add a column with the individual birth date
#'
#' @param x Table in the cdm that contains 'person_id' or 'subject_id'.
#' @param dateOfBirthName Name of the column to be added with the date of birth.
#' @param missingDay Day of the individuals with no or imposed day of birth.
#' @param missingMonth Month of the individuals with no or imposed month of
#' birth.
#' @param imposeDay Whether to impose day of birth.
#' @param imposeMonth Whether to impose month of birth.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return The function returns the table x with an extra column that contains
#' the date of birth.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDateOfBirth()
#' mockDisconnect(cdm = cdm)
#' }
addDateOfBirth <- function(x,
                           dateOfBirthName = "date_of_birth",
                           missingDay = 1,
                           missingMonth = 1,
                           imposeDay = FALSE,
                           imposeMonth = FALSE,
                           name = NULL) {
  comp <- newTable(name)

  joinPersonTable(
    x = x, dateOfBirthName = dateOfBirthName, missingDay = missingDay,
    missingMonth = missingMonth, imposeDay = imposeDay,
    imposeMonth = imposeMonth, genderConceptId = NULL
  ) |>
    dplyr::compute(name = comp$name, temporary = comp$temporary)
}

joinPersonTable <- function(x,
                            dateOfBirthName,
                            missingDay,
                            missingMonth,
                            imposeDay,
                            imposeMonth,
                            genderConceptId,
                            name) {
  cdm <- omopgenerics::cdmReference(x)

  personVariable <- checkX(x)

  person <- cdm$person

  if (!is.null(dateOfBirthName)) {
    person <- person %>%
      dplyr::filter(!is.na(.data$year_of_birth))

    # First add to person table
    if (imposeDay | !"day_of_birth" %in% colnames(person)) {
      person <- person %>%
        dplyr::mutate("day_of_birth" = .env$missingDay)
    } else {
      person <- person %>%
        dplyr::mutate("day_of_birth" = dplyr::if_else(
          is.na(.data$day_of_birth), .env$missingDay, .data$day_of_birth
        ))
    }
    # impose month
    if (imposeMonth | !"month_of_birth" %in% colnames(person)) {
      person <- person %>%
        dplyr::mutate("month_of_birth" = .env$missingMonth)
    } else {
      person <- person %>%
        dplyr::mutate("month_of_birth" = dplyr::if_else(
          is.na(.data$month_of_birth), .env$missingMonth, .data$month_of_birth
        ))
    }

    person <- person %>%
      dplyr::mutate(
        year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
        month_of_birth1 = as.character(as.integer(.data$month_of_birth)),
        day_of_birth1 = as.character(as.integer(.data$day_of_birth))
      ) %>%
      dplyr::mutate(!!dateOfBirthName := as.Date(paste0(
        .data$year_of_birth1, "-", .data$month_of_birth1, "-",
        .data$day_of_birth1
      )))
  }

  if (!is.null(genderConceptId)) {
    person <- person |>
      dplyr::select(
        !!personVariable := "person_id",
        dplyr::all_of(dateOfBirthName),
        !!genderConceptId := "gender_concept_id"
      )
  } else {
    person <- person |>
      dplyr::select(
        !!personVariable := "person_id", dplyr::all_of(dateOfBirthName)
      )
  }

  x <- x |>
    dplyr::left_join(person, by = personVariable)

  return(x)
}
