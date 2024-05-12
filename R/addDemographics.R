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

#' Compute demographic characteristics at a certain date
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the
#' demographics characteristics.
#' @param age TRUE or FALSE. If TRUE, age will be calculated relative to
#' indexDate.
#' @param ageMissingMonth Month of the year assigned to individuals with missing
#' month of birth.
#' @param ageName Age variable name.
#' @param ageMissingDay day of the month assigned to individuals
#' with missing day of birth.
#' @param ageImposeMonth TRUE or FALSE. Whether the month of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageImposeDay TRUE or FALSE. Whether the day of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageGroup if not NULL, a list of ageGroup vectors.
#' @param missingAgeGroupValue Value to include if missing age.
#' @param sex TRUE or FALSE. If TRUE, sex will be identified.
#' @param sexName Sex variable name.
#' @param missingSexValue Value to include if missing sex.
#' @param priorObservation TRUE or FALSE. If TRUE, days of between the start
#' of the current observation period and the indexDate will be calculated.
#' @param priorObservationName Prior observation variable name.
#' @param priorObservationType Whether to return a "date" or the number of
#' "days".
#' @param futureObservation TRUE or FALSE. If TRUE, days between the
#' indexDate and the end of the current observation period will be
#' calculated.
#' @param futureObservationName Future observation variable name.
#' @param futureObservationType Whether to return a "date" or the number of
#' "days".
#' @param dateOfBirth TRUE or FALSE, if true the date of birth will be return.
#' @param dateOfBirthName dateOfBirth column name.
#'
#' @return cohort table with the added demographic information columns.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDemographics()
#' mockDisconnect(cdm = cdm)
#' }
#'
addDemographics <- function(x,
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageName = "age",
                            ageMissingMonth = 1,
                            ageMissingDay = 1,
                            ageImposeMonth = FALSE,
                            ageImposeDay = FALSE,
                            ageGroup = NULL,
                            missingAgeGroupValue = "None",
                            sex = TRUE,
                            sexName = "sex",
                            missingSexValue = "None",
                            priorObservation = TRUE,
                            priorObservationName = "prior_observation",
                            priorObservationType = "days",
                            futureObservation = TRUE,
                            futureObservationName = "future_observation",
                            futureObservationType = "days",
                            dateOfBirth = FALSE,
                            dateOfBirthName = "date_of_birth") {
  ## change ageMissingMonth, ageMissingDay to integer

  cdm <- omopgenerics::cdmReference(x)

  ## check for standard types of user error
  personVariable <- checkX(x)
  checkCdm(cdm, c("person", "observation_period"))
  checkmate::assertLogical(age, any.missing = FALSE, len = 1)
  if (typeof(ageMissingMonth) == "character") {
    ageMissingMonth <- as.integer(ageMissingMonth)
  }
  if (typeof(ageMissingDay) == "character") {
    ageMissingDay <- as.integer(ageMissingDay)
  }
  checkmate::assertIntegerish(
    ageMissingMonth,
    lower = 1, upper = 12, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertIntegerish(
    ageMissingDay,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertLogical(ageImposeMonth, any.missing = FALSE, len = 1)
  checkmate::assertLogical(ageImposeDay, any.missing = FALSE, len = 1)
  ageGroup <- checkAgeGroup(ageGroup)
  checkmate::assertLogical(sex, any.missing = FALSE, len = 1)
  checkmate::assertLogical(priorObservation, any.missing = FALSE, len = 1)
  checkmate::assertLogical(futureObservation, any.missing = FALSE, len = 1)
  checkmate::assertLogical(dateOfBirth, any.missing = FALSE, len = 1)
  checkVariableInX(indexDate, x, !(age | priorObservation | futureObservation))
  if (!(age | sex | priorObservation | futureObservation | dateOfBirth)) {
    cli::cli_abort("age, sex, priorObservation, futureObservation and dateOfBirth can not be FALSE")
  }
  checkmate::assertCharacter(missingAgeGroupValue, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(missingSexValue, len = 1, any.missing = FALSE)
  assertChoice(priorObservationType, c("date", "days"), length = 1)
  assertChoice(futureObservationType, c("date", "days"), length = 1)

  # check variable names
  name <- character()
  if (age) {
    ageName <- checkSnakeCase(ageName)
    name <- c(name, ageName)
  }
  if (sex) {
    sexName <- checkSnakeCase(sexName)
    name <- c(name, sexName)
  }
  if (priorObservation) {
    priorObservationName <- checkSnakeCase(priorObservationName)
    name <- c(name, priorObservationName)
  }
  if (futureObservation) {
    futureObservationName <- checkSnakeCase(futureObservationName)
    name <- c(name, futureObservationName)
  }
  if (dateOfBirth) {
    dateOfBirthName <- checkSnakeCase(dateOfBirthName)
    name <- c(name, dateOfBirthName)
  }

  checkNewName(name = name, x = x)

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

  ids <- uniqueColumnName(
    c(colnames(cdm$person), colnames(cdm$observation_period), colnames(x)), 5
  )
  idBirth <- ids[1]
  idGender <- ids[2]
  idStart <- ids[3]
  idEnd <- ids[4]
  if (age == FALSE & !is.null(ageGroup)) {
    ageName <- ids[5]
  }

  if (priorObservation == TRUE || futureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    x <- x |>
      dplyr::left_join(
        x %>%
          dplyr::select(dplyr::all_of(c(personVariable, indexDate))) %>%
          dplyr::distinct() %>%
          dplyr::inner_join(
            cdm[["observation_period"]] %>%
              dplyr::rename(!!personVariable := "person_id") %>%
              dplyr::select(
                dplyr::all_of(personVariable),
                !!idStart := "observation_period_start_date",
                !!idEnd := "observation_period_end_date"
              ),
            by = personVariable
          ) %>%
          dplyr::filter(
            .data[[idStart]] <= .data[[indexDate]] &
              .data[[idEnd]] >= .data[[indexDate]]
          ),
        by = c(personVariable, indexDate)
      )
  }

  # update dates
  if (age | !is.null(ageGroup) | dateOfBirth | sex) {
    if (age == FALSE & dateOfBirth == FALSE & is.null(ageGroup)) {
      nm1 <- NULL
    } else {
      nm1 <- idBirth
    }
    if (sex) {
      nm2 <- idGender
    } else {
      nm2 <- NULL
    }
    x <- x %>%
      joinPersonTable(
        name = nm1,
        missingDay = ageMissingDay,
        missingMonth = ageMissingMonth,
        imposeDay = ageImposeDay,
        imposeMonth = ageImposeMonth,
        genderConceptId = nm2
      )
  }

  if (age == TRUE | !is.null(ageGroup)) {
    aQ <- glue::glue(
      'as.integer(floor(dbplyr::sql(CDMConnector::datediff(
    start = "{idBirth}", end = "{indexDate}", interval = "year"
    ))))'
    ) %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue(ageName))
  } else {
    aQ <- NULL
  }

  if (sex == TRUE) {
    sQ <- glue::glue(
      'dplyr::case_when(.data[["{idGender}"]] == 8507 ~ "Male",
    .data[["{idGender}"]] == 8532 ~ "Female",
      TRUE ~ "{missingSexValue}")'
    ) %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue(sexName))
  } else {
    sQ <- NULL
  }

  if (priorObservation == TRUE) {
    if (priorObservationType == "days") {
      pHQ <- glue::glue(
        'as.integer(local(CDMConnector::datediff("{idStart}","{indexDate}")))'
      )
    } else {
      pHQ <- glue::glue('.data[["{idStart}"]]')
    }
    pHQ <- pHQ %>%
      rlang::parse_exprs() %>%
      rlang::set_names(glue::glue(priorObservationName))
  } else {
    pHQ <- NULL
  }

  if (futureObservation == TRUE) {
    if (futureObservationType == "days") {
      fOQ <- glue::glue(
        'as.integer(local(CDMConnector::datediff("{indexDate}","{idEnd}")))'
      )
    } else {
      fOQ <- glue::glue('.data[["{idEnd}"]]')
    }
    fOQ <- fOQ |>
      rlang::parse_exprs() %>%
      rlang::set_names(futureObservationName)
  } else {
    fOQ <- NULL
  }

  if (dateOfBirth) {
    dobQ <- glue::glue('.data[["{idBirth}"]]') |>
      rlang::parse_exprs() |>
      rlang::set_names(dateOfBirthName)
  } else {
    dobQ <- NULL
  }

  x <- x %>%
    dplyr::mutate(!!!aQ, !!!sQ, !!!pHQ, !!!fOQ, !!!dobQ) |>
    dplyr::select(!dplyr::any_of(ids[1:4])) |>
    dplyr::compute()

  if (!is.null(ageGroup)) {
    x <- x |>
      addCategories(
        variable = ageName,
        categories = ageGroup,
        missingCategoryValue = missingAgeGroupValue
      )
    if (age == FALSE) {
      x <- x |> dplyr::select(-dplyr::all_of(ageName))
    }
  }

  return(x)
}

uniqueColumnName <- function(cols = character(), n = 1, nletters = 2) {
  x <- rep(list(letters), nletters) |>
    rlang::set_names(paste0("id_", seq_len(nletters)))
  tidyr::expand_grid(!!!x) |>
    tidyr::unite(col = "id", dplyr::starts_with("id_"), sep = "") |>
    dplyr::mutate("id" = paste0("id_", .data$id)) |>
    dplyr::filter(!.data$id %in% .env$cols) |>
    dplyr::sample_n(size = .env$n) |>
    dplyr::pull("id")
}

#' Compute the age of the individuals at a certain date
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the age.
#' @param ageName Name of the new column that contains age.
#' @param ageGroup List of age groups to be added.
#' @param ageMissingMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageMissingDay day of the month assigned to individuals with missing
#' day of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be
#' considered as missing for all the individuals.
#' @param ageImposeDay Whether the day of the date of birth will be considered
#' as missing for all the individuals.
#' @param missingAgeGroupValue Value to include if missing age.
#'
#' @return tibble with the age column added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   addAge()
#' mockDisconnect(cdm = cdm)
#' }
addAge <- function(x,
                   indexDate = "cohort_start_date",
                   ageName = "age",
                   ageGroup = NULL,
                   ageMissingMonth = 1,
                   ageMissingDay = 1,
                   ageImposeMonth = FALSE,
                   ageImposeDay = FALSE,
                   missingAgeGroupValue = "None") {
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = TRUE,
      ageName = ageName,
      ageGroup = ageGroup,
      ageMissingDay = ageMissingDay,
      ageMissingMonth = ageMissingMonth,
      ageImposeDay = ageImposeDay,
      ageImposeMonth = ageImposeMonth,
      missingAgeGroupValue = missingAgeGroupValue,
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
#' @param indexDate Variable in x that contains the date to compute the future
#' observation.
#' @param futureObservationName name of the new column to be added.
#' @param futureObservationType Whether to return a "date" or the number of
#' "days".
#'
#' @return cohort table with added column containing future observation of the
#' individuals.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addFutureObservation()
#' mockDisconnect(cdm = cdm)
#' }
addFutureObservation <- function(x,
                                 indexDate = "cohort_start_date",
                                 futureObservationName = "future_observation",
                                 futureObservationType = "days") {
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageMissingDay = NULL,
      ageMissingMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = TRUE,
      futureObservationName = futureObservationName,
      futureObservationType = futureObservationType,
      ageName = NULL,
      sexName = NULL,
      priorObservationName = NULL
    )

  return(x)
}

#' Compute the number of days of prior observation in the current observation period
#' at a certain date
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the prior
#' observation.
#' @param priorObservationName name of the new column to be added.
#' @param priorObservationType Whether to return a "date" or the number of
#' "days".
#'
#' @return cohort table with added column containing prior observation of the
#' individuals.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addPriorObservation()
#' mockDisconnect(cdm = cdm)
#' }
addPriorObservation <- function(x,
                                indexDate = "cohort_start_date",
                                priorObservationName = "prior_observation",
                                priorObservationType = "days") {
  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      ageGroup = NULL,
      ageMissingDay = NULL,
      ageMissingMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      priorObservationName = priorObservationName,
      priorObservationType = priorObservationType,
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
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param window window to consider events of.
#' @param completeInterval If the individuals are in observation for the full window.
#' @param nameStyle Name of the new columns to create, it must contain
#' "window_name" if multiple windows are provided.
#'
#' @return cohort table with the added binary column assessing inObservation.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addInObservation()
#' mockDisconnect(cdm = cdm)
#' }
#'
addInObservation <- function(x,
                             indexDate = "cohort_start_date",
                             window = c(0, 0),
                             completeInterval = FALSE,
                             nameStyle = "in_observation") {
  if (!is.list(window)) {
    window <- list(window)
  }

  ## check for standard types of user error
  cdm <- omopgenerics::cdmReference(x)
  personVariable <- checkX(x)
  checkCdm(cdm, c("observation_period"))
  checkVariableInX(indexDate, x)
  checkWindow(window)
  names(window) <- getWindowNames(window)
  assertNameStyle(nameStyle = nameStyle, values = list("window_name" = window))
  overwriteCols <- glue::glue(nameStyle, window_name = names(window))
  overwriteCols <- overwriteCols[overwriteCols %in% colnames(x)]
  if (length(overwriteCols) > 0) {
    cli::cli_warn(c("!" = "{overwriteCols} column{?s} will be overwritten"))
    x <- x |>
      dplyr::select(!dplyr::all_of(overwriteCols))
  }

  ids <- uniqueColumnName(
    c(colnames(cdm$person), colnames(cdm$observation_period), colnames(x)), 2, 3
  )
  idPrior <- ids[1]
  idFuture <- ids[2]

  x <- x %>%
    addDemographics(
      indexDate = indexDate,
      age = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
      priorObservationName = idPrior,
      futureObservation = TRUE,
      futureObservationName = idFuture
    ) |>
    dplyr::mutate(!!idPrior := -.data[[idPrior]])

  for (k in seq_along(window)) {
    win <- window[[k]]
    window_name <- names(window)[k]
    nam <- glue::glue(nameStyle) |> as.character()

    if (all(win == c(0, 0))) {
      x <- x %>%
        dplyr::mutate(!!nam := as.numeric(
          dplyr::if_else(is.na(.data[[idPrior]]), 0, 1)
        ))
    } else {
      lower <- win[1]
      upper <- win[2]

      if (completeInterval == TRUE) {
        if (is.infinite(lower) | is.infinite(upper)) {
          x <- x %>% dplyr::mutate(!!nam := 0)
        } else {
          x <- x %>%
            dplyr::mutate(!!nam := as.numeric(dplyr::if_else(
              !is.na(.data[[idPrior]]) &
                .data[[idPrior]] <= .env$lower &
                .env$upper <= .data[[idFuture]],
              1,
              0
            )))
        }
      } else {
        if (is.infinite(lower)) {
          if (is.infinite(upper)) {
            x <- x %>%
              dplyr::mutate(!!nam := as.numeric(dplyr::if_else(
                !is.na(.data[[idPrior]]), 1, 0
              )))
          } else {
            x <- x %>%
              dplyr::mutate(!!nam := as.numeric(dplyr::if_else(
                !is.na(.data[[idPrior]]) &
                  .data[[idPrior]] <= .env$upper,
                1,
                0
              )))
          }
        } else {
          if (is.infinite(upper)) {
            x <- x %>%
              dplyr::mutate(!!nam := as.numeric(dplyr::if_else(
                !is.na(.data[[idPrior]]) &
                  .env$lower <= .data[[idFuture]],
                1,
                0
              )))
          } else {
            x <- x %>%
              dplyr::mutate(!!nam := as.numeric(dplyr::if_else(
                !is.na(.data[[idPrior]]) &
                  .data[[idPrior]] <= .env$upper &
                  .env$lower <= .data[[idFuture]],
                1,
                0
              )))
          }
        }
      }
    }
  }

  x <- x |>
    dplyr::select(!dplyr::all_of(ids)) |>
    dplyr::compute()

  return(x)
}

#' Compute the sex of the individuals
#'
#' @param x Table with individuals in the cdm.
#' @param sexName name of the new column to be added.
#' @param missingSexValue Value to include if missing sex.
#'
#' @return table x with the added column with sex information.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addSex()
#' mockDisconnect(cdm = cdm)
#' }
#'
addSex <- function(x,
                   sexName = "sex",
                   missingSexValue = "None") {
  x <- x %>%
    addDemographics(
      indexDate = NULL,
      age = FALSE,
      ageGroup = NULL,
      ageMissingDay = NULL,
      ageMissingMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = TRUE,
      sexName = sexName,
      missingSexValue = missingSexValue,
      priorObservation = FALSE,
      futureObservation = FALSE,
      ageName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL
    )

  return(x)
}
