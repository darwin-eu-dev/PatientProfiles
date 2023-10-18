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

#' Compute demographic characteristics at a certain date
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the
#' demographics characteristics.
#' @param age TRUE or FALSE. If TRUE, age will be calculated relative to
#' indexDate
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth.
#' @param ageName Age variable name
#' @param ageDefaultDay day of the month assigned to individuals
#' with missing day of birth.
#' @param ageImposeMonth TRUE or FALSE. Whether the month of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageImposeDay TRUE or FALSE. Whether the day of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageGroup if not NULL, a list of ageGroup vectors
#' @param sex TRUE or FALSE. If TRUE, sex will be identified
#' @param sexName Sex variable name
#' @param priorObservation TRUE or FALSE. If TRUE, days of between the start
#' of the current observation period and the indexDate will be calculated
#' @param priorObservationName Prior observation variable name
#' @param futureObservation TRUE or FALSE. If TRUE, days between the
#' indexDate and the end of the current observation period will be
#' calculated
#' @param futureObservationName Future observation variable name
#'
#' @return cohort table with the added demographic information columns
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% addDemographics(cdm)
#' }
#'
addDemographics <- function(x,
                            cdm = attr(x, "cdm_reference"),
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageName = "age",
                            ageDefaultMonth = 1,
                            ageDefaultDay = 1,
                            ageImposeMonth = FALSE,
                            ageImposeDay = FALSE,
                            ageGroup = NULL,
                            sex = TRUE,
                            sexName = "sex",
                            priorObservation = TRUE,
                            priorObservationName = "prior_observation",
                            futureObservation = TRUE,
                            futureObservationName = "future_observation") {
  ## change ageDefaultMonth, ageDefaultDay to integer

  if (typeof(ageDefaultMonth) == "character") {
    ageDefaultMonth <- as.integer(ageDefaultMonth)
  }

  if (typeof(ageDefaultDay) == "character") {
    ageDefaultDay <- as.integer(ageDefaultDay)
  }

  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("person", "observation_period"))
  checkmate::assertLogical(age, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(
    ageDefaultMonth,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertIntegerish(
    ageDefaultDay,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertLogical(ageImposeMonth, any.missing = FALSE, len = 1)
  checkmate::assertLogical(ageImposeDay, any.missing = FALSE, len = 1)
  ageGroup <- checkAgeGroup(ageGroup)
  checkmate::assertLogical(sex, any.missing = FALSE, len = 1)
  checkmate::assertLogical(priorObservation, any.missing = FALSE, len = 1)
  checkmate::assertLogical(futureObservation, any.missing = FALSE, len = 1)
  checkVariableInX(indexDate, x, !(age | priorObservation | futureObservation))
  if (!(age | sex | priorObservation | futureObservation)) {
    cli::cli_abort("age, sex, priorObservation, futureObservation can not be FALSE")
  }

  # check variable names
  if (age) {
    ageName <- checkSnakeCase(ageName)
  }
  if (sex) {
    sexName <- checkSnakeCase(sexName)
  }
  if (priorObservation) {
    priorObservationName <- checkSnakeCase(priorObservationName)
  }
  if (futureObservation) {
    futureObservationName <- checkSnakeCase(futureObservationName)
  }

  checkNewName(ageName, x)
  checkNewName(sexName, x)
  checkNewName(priorObservationName, x)
  checkNewName(futureObservationName, x)

  if (age == TRUE || priorObservation == TRUE || futureObservation == TRUE) {
    checkmate::assert_true(
      inherits(
        x %>%
          utils::head(1) %>%
          dplyr::pull(indexDate),
        c("Date", "POSIXt")
      )
    )
  }

  # Start code
  startTibble <- x
  startNames <- colnames(x)

  personDetails <- cdm[["person"]] %>%
    dplyr::select(
      "person_id",
      "gender_concept_id",
      "year_of_birth",
      "month_of_birth",
      "day_of_birth"
    ) %>%
    dplyr::rename(!!personVariable := "person_id")

  if (priorObservation == TRUE || futureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    obsPeriodDetails <- x %>%
      dplyr::select(dplyr::all_of(c(personVariable, indexDate))) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(
        cdm[["observation_period"]] %>%
          dplyr::rename(!!personVariable := "person_id") %>%
          dplyr::select(
            dplyr::all_of(personVariable),
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = personVariable
      ) %>%
      dplyr::filter(.data$observation_period_start_date <=
        .data[[indexDate]] &
        .data$observation_period_end_date >=
          .data[[indexDate]])
  }

  # update dates
  if (age) {
    personDetails <- personDetails %>%
      dplyr::filter(!is.na(.data$year_of_birth)) %>%
      addDateOfBirth(
        name = "date_of_birth",
        missingDay = ageDefaultDay,
        missingMonth = ageDefaultMonth,
        imposeDay = ageImposeDay,
        imposeMonth = ageImposeMonth
      )
  }

  # join if not the person table
  if (any(!c("person_id", "gender_concept_id") %in% colnames(x))) {
    x <- x %>%
      dplyr::left_join(
        personDetails %>%
          dplyr::select(dplyr::any_of(c(
            personVariable,
            "date_of_birth",
            "gender_concept_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ))),
        by = personVariable
      )
  }

  if (priorObservation == TRUE || futureObservation == TRUE) {
    x <- x %>%
      dplyr::left_join(obsPeriodDetails,
        by = c(personVariable, indexDate)
      )
  }

  if (age == TRUE) {
    aQ <- ageQuery(indexDate, name = ageName)
  } else {
    aQ <- NULL
  }

  if (sex == TRUE) {
    sQ <- sexQuery(name = sexName)
  } else {
    sQ <- NULL
  }

  if (priorObservation == TRUE) {
    pHQ <- priorObservationQuery(indexDate, name = priorObservationName)
  } else {
    pHQ <- NULL
  }

  if (futureObservation == TRUE) {
    fOQ <- futureObservationQuery(indexDate, name = futureObservationName)
  } else {
    fOQ <- NULL
  }

  x <- x %>%
    dplyr::mutate(
      !!!aQ,
      !!!sQ,
      !!!pHQ,
      !!!fOQ
    )

  x <- x %>%
    dplyr::select(
      dplyr::all_of(startNames),
      dplyr::any_of(c(
        ageName, sexName,
        priorObservationName,
        futureObservationName
      ))
    )

  if (sex == TRUE) {
    x <- x %>%
      dplyr::mutate(!!sexName := dplyr::if_else(!is.na(.data[[sexName]]),
        .data[[sexName]],
        "None"
      ))
  }

  x <- x %>%
    CDMConnector::computeQuery()

  if (!is.null(ageGroup)) {
    x <- addCategories(
      x = x,
      variable = ageName,
      categories = ageGroup,
      missingCategoryValue = "None"
    )
  }

  # put back the initial attributes to the output tibble
  x <- x %>% addAttributes(startTibble)

  return(x)
}



ageQuery <- function(indexDate, name) {
  return(glue::glue('floor(dbplyr::sql(
    CDMConnector::datediff(
      start = "date_of_birth",
      end = "{indexDate}",
      interval = "year"
    )
  ))') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

sexQuery <- function(name) {
  return(glue::glue('dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA))') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

priorObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("observation_period_start_date",
                      "{indexDate}")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

futureObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("{indexDate}",
                          "observation_period_end_date")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

#' Compute the age of the individuals at a certain date
#'
#' @param x Table with individuals in the cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the age.
#' @param ageName Name of the new column that contains age.
#' @param ageGroup List of age groups to be added.
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageDefaultDay day of the month assigned to individuals with missing
#' day of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be
#' considered as missing for all the individuals.
#' @param ageImposeDay Whether the day of the date of birth will be considered
#' as missing for all the individuals.
#'
#' @return tibble with the age column added
#' @export
#'
#' @examples
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(PatientProfiles)
#' cohort1 <- dplyr::tibble(
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
#' person <- dplyr::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8532", "8507"),
#'   year_of_birth = c(2000, 1995, NA),
#'   month_of_birth = c(NA, 07, 08),
#'   day_of_birth = c(01, 25, 03)
#' )
#' cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)
#' addAge(x = cdm[["cohort1"]], cdm = cdm)
#' }
addAge <- function(x,
                   cdm = attr(x, "cdm_reference"),
                   indexDate = "cohort_start_date",
                   ageName = "age",
                   ageGroup = NULL,
                   ageDefaultMonth = 1,
                   ageDefaultDay = 1,
                   ageImposeMonth = FALSE,
                   ageImposeDay = FALSE) {
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = indexDate,
      age = TRUE,
      ageName = ageName,
      ageGroup = ageGroup,
      ageDefaultDay = ageDefaultDay,
      ageDefaultMonth = ageDefaultMonth,
      ageImposeDay = ageImposeDay,
      ageImposeMonth = ageImposeMonth,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE,
      sexName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL
    )

  return(x)
}

#' Compute the number of days till the end of the observation period at a
#' certain date
#'
#' @param x Table with individuals in the cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the future
#' observation.
#' @param futureObservationName name of the new column to be added
#'
#' @return cohort table with added column containing future observation of the
#' individuals
#' @export
#'
#' @examples
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(PatientProfiles)
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"),
#'     as.Date("2010-03-01"),
#'     as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"),
#'     as.Date("2013-01-01"),
#'     as.Date("2013-01-01")
#'   )
#' )
#'
#' obs_1 <- dplyr::tibble(
#'   observation_period_id = c("1", "2", "3"),
#'   person_id = c("1", "2", "3"),
#'   observation_period_start_date = c(
#'     as.Date("2010-02-03"),
#'     as.Date("2010-02-01"),
#'     as.Date("2010-01-01")
#'   ),
#'   observation_period_end_date = c(
#'     as.Date("2014-01-01"),
#'     as.Date("2012-01-01"),
#'     as.Date("2012-01-01")
#'   )
#' )
#'
#' cdm <-
#'   mockPatientProfiles(
#'     seed = 1,
#'     cohort1 = cohort1,
#'     observation_period = obs_1
#'   )
#'
#' result <- cdm$cohort1 %>% addFutureObservation(cdm)
#' }
addFutureObservation <- function(x,
                                 cdm = attr(x, "cdm_reference"),
                                 indexDate = "cohort_start_date",
                                 futureObservationName = "future_observation") {
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = TRUE,
      futureObservationName = futureObservationName,
      ageName = NULL,
      sexName = NULL,
      priorObservationName = NULL
    )

  return(x)
}

#' Compute the number of days of prior observation in the current observation period
#' at a certain date
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the prior
#' observation.
#' @param priorObservationName name of the new column to be added
#'
#' @return cohort table with added column containing prior observation of the
#' individuals
#' @export
#'
#' @examples
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(PatientProfiles)
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"),
#'     as.Date("2010-03-01"),
#'     as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"),
#'     as.Date("2013-01-01"),
#'     as.Date("2013-01-01")
#'   )
#' )
#'
#' obs_1 <- dplyr::tibble(
#'   observation_period_id = c("1", "2", "3"),
#'   person_id = c("1", "2", "3"),
#'   observation_period_start_date = c(
#'     as.Date("2010-02-03"),
#'     as.Date("2010-02-01"),
#'     as.Date("2010-01-01")
#'   ),
#'   observation_period_end_date = c(
#'     as.Date("2014-01-01"),
#'     as.Date("2012-01-01"),
#'     as.Date("2012-01-01")
#'   )
#' )
#'
#' cdm <-
#'   mockPatientProfiles(
#'     seed = 1,
#'     cohort1 = cohort1,
#'     observation_period = obs_1
#'   )
#'
#' result <- cdm$cohort1 %>% addPriorObservation(cdm)
#' }
addPriorObservation <- function(x,
                                cdm = attr(x, "cdm_reference"),
                                indexDate = "cohort_start_date",
                                priorObservationName = "prior_observation") {
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      priorObservationName = priorObservationName,
      futureObservation = FALSE,
      ageName = NULL,
      sexName = NULL,
      futureObservationName = NULL
    )

  return(x)
}

#' Indicate if a certain record is within the observation period
#'
#' @param x Table with individuals in the cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param name name of the column to hold the result of the query:
#' 1 if the individual is in observation, 0 if not
#'
#' @return cohort table with the added binary column assessing inObservation
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% addInObservation(cdm)
#' }
#'
addInObservation <- function(x,
                             cdm = attr(x, "cdm_reference"),
                             indexDate = "cohort_start_date",
                             name = "in_observation") {
  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("observation_period"))
  checkVariableInX(indexDate, x)
  checkmate::assertCharacter(name, any.missing = FALSE, len = 1)
  name <- checkNewName(name, x)

  # Start code
  name <- rlang::enquo(name)

  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = indexDate,
      age = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = TRUE
    ) %>%
    dplyr::mutate(
      !!name := as.numeric(dplyr::if_else(
        is.na(.data$prior_observation) | is.na(.data$future_observation) | .data$prior_observation < 0 | .data$future_observation < 0, 0, 1
      ))
    ) %>%
    dplyr::select(
      -"prior_observation", -"future_observation"
    )

  x <- x %>%
    CDMConnector::computeQuery()

  return(x)
}

#' Compute the sex of the individuals
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param sexName name of the new column to be added
#'
#' @return table x with the added column with sex information
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% addSex(cdm)
#' }
#'
addSex <- function(x,
                   cdm = attr(x, "cdm_reference"),
                   sexName = "sex") {
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = NULL,
      age = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = TRUE,
      sexName = sexName,
      priorObservation = FALSE,
      futureObservation = FALSE,
      ageName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL
    )

  return(x)
}
