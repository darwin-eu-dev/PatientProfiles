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

#' Query to add demographic characteristics at a certain date
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addDemographics()`, except query is not computed to a table.
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
#'   addDemographicsQuery()
#' mockDisconnect(cdm = cdm)
#' }
#'
addDemographicsQuery <- function(x,
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
  x |>
    .addDemographicsQuery(
      indexDate = indexDate,
      age = age,
      ageGroup = ageGroup,
      ageMissingDay = ageMissingDay,
      ageMissingMonth = ageMissingMonth,
      ageImposeDay = ageImposeDay,
      ageImposeMonth = ageImposeMonth,
      sex = sex,
      sexName = sexName,
      missingSexValue = missingSexValue,
      priorObservation = priorObservation,
      futureObservation = futureObservation,
      ageName = ageName,
      priorObservationName = priorObservationName,
      futureObservationName = futureObservationName,
      missingAgeGroupValue = missingAgeGroupValue,
      priorObservationType = priorObservationType,
      futureObservationType = futureObservationType,
      dateOfBirth = dateOfBirth,
      dateOfBirthName = dateOfBirthName
    )
}

#' Query to add the age of the individuals at a certain date
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addAge()`, except query is not computed to a table.
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
#'   addAgeQuery()
#' mockDisconnect(cdm = cdm)
#' }
addAgeQuery <- function(x,
                        indexDate = "cohort_start_date",
                        ageName = "age",
                        ageGroup = NULL,
                        ageMissingMonth = 1,
                        ageMissingDay = 1,
                        ageImposeMonth = FALSE,
                        ageImposeDay = FALSE,
                        missingAgeGroupValue = "None") {
  x |>
    .addDemographicsQuery(
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
      futureObservationName = NULL,
      missingSexValue = NULL,
      priorObservationType = NULL,
      futureObservationType = NULL,
      dateOfBirth = FALSE,
      dateOfBirthName = NULL
    )
}

#' Query to add the number of days till the end of the observation period at a
#' certain date
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addFutureObservation()`, except query is not computed to a table.
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
#'   addFutureObservationQuery()
#' mockDisconnect(cdm = cdm)
#' }
addFutureObservationQuery <- function(x,
                                      indexDate = "cohort_start_date",
                                      futureObservationName = "future_observation",
                                      futureObservationType = "days") {
  x |>
    .addDemographicsQuery(
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
      priorObservationName = NULL,
      missingAgeGroupValue = NULL,
      missingSexValue = NULL,
      priorObservationType = NULL,
      dateOfBirth = FALSE,
      dateOfBirthName = NULL
    )
}

#' Query to add the number of days of prior observation in the current
#' observation period at a certain date
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addPriorObservation()`, except query is not computed to a table.
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
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   addPriorObservationQuery()
#' mockDisconnect(cdm = cdm)
#' }
addPriorObservationQuery <- function(x,
                                     indexDate = "cohort_start_date",
                                     priorObservationName = "prior_observation",
                                     priorObservationType = "days") {
  x |>
    .addDemographicsQuery(
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
      futureObservationName = NULL,
      missingAgeGroupValue = NULL,
      missingSexValue = NULL,
      futureObservationType = NULL,
      dateOfBirth = FALSE,
      dateOfBirthName = NULL
    )
}

#' Query to add the sex of the individuals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addSex()`, except query is not computed to a table.
#'
#' @param x Table with individuals in the cdm.
#' @param sexName name of the new column to be added.
#' @param missingSexValue Value to include if missing sex.
#'
#' @return table x with the added column with sex information.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addSexQuery()
#' mockDisconnect(cdm = cdm)
#' }
#'
addSexQuery <- function(x,
                        sexName = "sex",
                        missingSexValue = "None") {
  x |>
    .addDemographicsQuery(
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
      futureObservationName = NULL,
      missingAgeGroupValue = NULL,
      priorObservationType = NULL,
      futureObservationType = NULL,
      dateOfBirth = FALSE,
      dateOfBirthName = NULL
    )
}

#' Query to add a column with the individual birth date
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addDateOfBirth()`, except query is not computed to a table.
#'
#' @param x Table in the cdm that contains 'person_id' or 'subject_id'.
#' @param dateOfBirthName Name of the column to be added with the date of birth.
#' @param missingDay Day of the individuals with no or imposed day of birth.
#' @param missingMonth Month of the individuals with no or imposed month of
#' birth.
#' @param imposeDay Whether to impose day of birth.
#' @param imposeMonth Whether to impose month of birth.
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
#'   addDateOfBirthQuery()
#' mockDisconnect(cdm = cdm)
#' }
addDateOfBirthQuery <- function(x,
                                dateOfBirthName = "date_of_birth",
                                missingDay = 1,
                                missingMonth = 1,
                                imposeDay = FALSE,
                                imposeMonth = FALSE) {
  x |>
    .addDemographicsQuery(
      indexDate = NULL,
      age = FALSE,
      ageGroup = NULL,
      ageMissingDay = missingDay,
      ageMissingMonth = missingMonth,
      ageImposeDay = imposeDay,
      ageImposeMonth = imposeMonth,
      sex = FALSE,
      sexName = NULL,
      missingSexValue = NULL,
      priorObservation = FALSE,
      futureObservation = FALSE,
      ageName = NULL,
      priorObservationName = NULL,
      futureObservationName = NULL,
      missingAgeGroupValue = NULL,
      priorObservationType = NULL,
      futureObservationType = NULL,
      dateOfBirth = TRUE,
      dateOfBirthName = dateOfBirthName
    )
}

.addDemographicsQuery <- function(x,
                                  indexDate,
                                  age,
                                  ageName,
                                  ageMissingMonth,
                                  ageMissingDay,
                                  ageImposeMonth,
                                  ageImposeDay,
                                  ageGroup,
                                  missingAgeGroupValue,
                                  sex,
                                  sexName,
                                  missingSexValue,
                                  priorObservation,
                                  priorObservationName,
                                  priorObservationType,
                                  futureObservation,
                                  futureObservationName,
                                  futureObservationType,
                                  dateOfBirth,
                                  dateOfBirthName,
                                  call = parent.frame()) {
  # initial checks
  x <- validateX(x, call = call)
  age <- validateLogical(age, call = call)
  sex <- validateLogical(sex, call = call)
  priorObservation <- validateLogical(priorObservation, call = call)
  futureObservation <- validateLogical(futureObservation, call = call)
  dateOfBirth <- validateLogical(dateOfBirth, call = call)
  ageGroup <- omopgenerics::validateAgeGroupArgument(ageGroup, call = call)
  notIndexDate <- !any(c(
    age, !is.null(ageGroup), priorObservation, futureObservation
  ))
  indexDate <- validateIndexDate(indexDate, null = notIndexDate, x = x, call = call)
  ageName <- validateColumn(ageName, null = !age, call = call)
  sexName <- validateColumn(sexName, null = !sex, call = call)
  priorObservationName <- validateColumn(priorObservationName, null = !priorObservation, call = call)
  futureObservationName <- validateColumn(futureObservationName, null = !futureObservation, call = call)
  dateOfBirthName <- validateColumn(dateOfBirthName, null = !dateOfBirth, call = call)
  noAge <- !age & length(ageGroup) == 0 & !dateOfBirth
  ageMissingMonth <- validateAgeMissingMonth(ageMissingMonth, null = noAge, call = call)
  ageMissingDay <- validateAgeMissingDay(ageMissingDay, null = noAge, call = call)
  ageImposeMonth <- validateLogical(ageImposeMonth, null = noAge, call = call)
  ageImposeDay <- validateLogical(ageImposeDay, null = noAge, call = call)
  missingAgeGroupValue <- validateMissingValue(missingAgeGroupValue, null = !age & length(ageGroup) == 0, call = call)
  missingSexValue <- validateMissingValue(missingSexValue, null = !sex, call = call)
  priorObservationType <- validateType(priorObservationType, null = !priorObservation, call = call)
  futureObservationType <- validateType(futureObservationType, null = !futureObservation, call = call)

  # if no new columns return x
  if (!(age | sex | priorObservation | futureObservation | dateOfBirth | !is.null(ageGroup))) {
    return(x)
  }

  newColumns <- c(ageName, names(ageGroup), sexName, priorObservationName, futureObservationName, dateOfBirthName)
  toEliminate <- newColumns[newColumns %in% colnames(x)]
  if (length(toEliminate) > 0) {
    cli::cli_warn(c("!" = "The following columns will be overwritten: {toEliminate}"))
    x <- x |> dplyr::select(!dplyr::all_of(toEliminate))
  }

  if (!identical(unique(newColumns), newColumns)) {
    cli::cli_abort("Names of new columns must be unique: {newColumns}.", call = call)
  }

  cdm <- omopgenerics::cdmReference(x)

  personVariable <- c("person_id", "subject_id")
  personVariable <- personVariable[personVariable %in% colnames(x)]

  # OBSERVATION PERIOD JOIN
  if (priorObservation || futureObservation) {
    # prior observation
    if (priorObservation == TRUE) {
      if (priorObservationType == "days") {
        pHQ <- glue::glue(
          'as.integer(local(CDMConnector::datediff("start_date","{indexDate}")))'
        )
      } else {
        pHQ <- ".data$start_date"
      }
      pHQ <- pHQ %>%
        rlang::parse_exprs() %>%
        rlang::set_names(glue::glue(priorObservationName))
    } else {
      pHQ <- NULL
    }

    # future observation
    if (futureObservation == TRUE) {
      if (futureObservationType == "days") {
        fOQ <- glue::glue(
          'as.integer(local(CDMConnector::datediff("{indexDate}","end_date")))'
        )
      } else {
        fOQ <- ".data$end_date"
      }
      fOQ <- fOQ |>
        rlang::parse_exprs() %>%
        rlang::set_names(futureObservationName)
    } else {
      fOQ <- NULL
    }

    observationPeriod <- x |>
      dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
      dplyr::distinct() |>
      dplyr::inner_join(
        cdm[["observation_period"]] |>
          dplyr::select(
            !!personVariable := "person_id",
            "start_date" = "observation_period_start_date",
            "end_date" = "observation_period_end_date"
          ),
        by = personVariable
      ) |>
      dplyr::filter(
        .data$start_date <= .data[[indexDate]] &
          .data$end_date >= .data[[indexDate]]
      ) %>%
      dplyr::mutate(!!!pHQ, !!!fOQ) |>
      dplyr::select(dplyr::all_of(c(
        personVariable, indexDate, priorObservationName, futureObservationName
      )))

    xnew <- x |>
      dplyr::left_join(observationPeriod, by = c(personVariable, indexDate))
  } else {
    xnew <- x
  }

  # PERSON TABLE JOIN
  if (age | !is.null(ageGroup) | dateOfBirth | sex) {
    if (!age && is.null(ageGroup) && !dateOfBirth) indexDate <- NULL

    person <- x |>
      dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
      dplyr::distinct() |>
      dplyr::inner_join(
        cdm$person |>
          dplyr::select(
            !!personVariable := "person_id", "gender_concept_id",
            "year_of_birth", dplyr::any_of(c("month_of_birth", "day_of_birth"))
          ),
        by = personVariable
      )

    # AGE QUERYS
    if (age | !is.null(ageGroup) | dateOfBirth) {
      if (!"day_of_birth" %in% colnames(person) || isTRUE(ageImposeDay)) {
        dB <- ".env$ageMissingDay"
      } else {
        dB <- "dplyr::if_else(is.na(.data$day_of_birth), .env$ageMissingDay, .data$day_of_birth)"
      }
      if (!"month_of_birth" %in% colnames(person) || isTRUE(ageImposeMonth)) {
        mB <- ".env$ageMissingMonth"
      } else {
        mB <- "dplyr::if_else(is.na(.data$month_of_birth), .env$ageMissingMonth, .data$month_of_birth)"
      }
      if (!dateOfBirth) dateOfBirthName <- "date_of_birth"
      dtBQ <- "dplyr::if_else(
        is.na(.data$year_of_birth),
        as.Date(NA),
        as.Date(paste0(as.character(as.integer(.data$year_of_birth)), '-',
          as.character(as.integer({mB})), '-', as.character(as.integer({dB}))))
      )" |>
        glue::glue() |>
        rlang::parse_exprs() |>
        rlang::set_names(dateOfBirthName)
      if (age || length(ageGroup) > 0) {
        if (!age) ageName <- "age"
        aQ <- "as.integer(local(CDMConnector::datediff('{dateOfBirthName}', '{indexDate}', interval = 'year')))" |>
          glue::glue() |>
          rlang::parse_exprs() |>
          rlang::set_names(ageName)
        if (length(ageGroup) > 0) {
          agQ <- ageGroupQuery(ageName, ageGroup, missingAgeGroupValue)
        } else {
          agQ <- NULL
        }
      } else {
        aQ <- NULL
        agQ <- NULL
      }
    } else {
      dBQ <- NULL
      mBQ <- NULL
      dtBQ <- NULL
      aQ <- NULL
      agQ <- NULL
    }
    if (sex == TRUE) {
      sQ <- 'dplyr::case_when(.data$gender_concept_id == 8507 ~ "Male",
          .data$gender_concept_id == 8532 ~ "Female"'
      if (is.na(missingSexValue)) {
        sQ <- glue::glue("{sQ}, .default = NA_character_)")
      } else {
        sQ <- glue::glue('{sQ}, .default = "{missingSexValue}")')
      }
      sQ <- sQ |>
        rlang::parse_exprs() |>
        rlang::set_names(sexName)
    } else {
      sQ <- NULL
    }

    # variables to select
    newColumns2 <- c(
      ageName[age], names(ageGroup), sexName, dateOfBirthName[dateOfBirth]
    )

    person <- person %>%
      dplyr::mutate(!!!dtBQ) %>%
      dplyr::mutate(!!!c(aQ, agQ, sQ)) |>
      dplyr::select(dplyr::all_of(c(personVariable, indexDate, newColumns2)))

    xnew <- xnew |>
      dplyr::left_join(person, by = c(personVariable, indexDate))
  }

  xnew <- xnew |>
    dplyr::select(dplyr::all_of(c(colnames(x), newColumns)))


  return(xnew)
}

ageGroupQuery <- function(ageName, ageGroup, missingAgeGroupValue) {
  ageName <- paste0(".data[['", ageName, "']]")
  lapply(seq_along(ageGroup), function(i) {
    xx <- lapply(seq_along(ageGroup[[i]]), function(k) {
      if (is.infinite(ageGroup[[i]][[k]][2])) {
        paste0(
          ageName, " >= ", ageGroup[[i]][[k]][1], "L ~ '",
          names(ageGroup[[i]])[k], "'"
        )
      } else {
        paste0(
          ageName, " >= ", ageGroup[[i]][[k]][1], "L && ", ageName, "<= ",
          ageGroup[[i]][[k]][2], "L ~ '", names(ageGroup[[i]])[k], "'"
        )
      }
    }) |>
      unlist()
    if (is.na(missingAgeGroupValue)) {
      xx <- c(xx, ".default = NA_character_")
    } else {
      xx <- c(xx, paste0('.default = "', missingAgeGroupValue, '"'))
    }
    xx <- paste0(xx, collapse = ", ")
    paste0("dplyr::case_when(", xx, ")")
  }) |>
    unlist() |>
    rlang::parse_exprs() |>
    rlang::set_names(names(ageGroup))
}

#' Query to add a new column to indicate if a certain record is within the
#' observation period
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Same as `addInObservation()`, except query is not computed to a table.
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
#'   addInObservationQuery()
#' mockDisconnect(cdm = cdm)
#' }
#'
addInObservationQuery <- function(x,
                                  indexDate = "cohort_start_date",
                                  window = c(0, 0),
                                  completeInterval = FALSE,
                                  nameStyle = "in_observation") {

  x |>
    .addInObservationQuery(
      indexDate = indexDate,
      window = window,
      completeInterval = completeInterval,
      nameStyle = nameStyle
    )
}

.addInObservationQuery <- function(x,
                                   indexDate = "cohort_start_date",
                                   window = c(0, 0),
                                   completeInterval = FALSE,
                                   nameStyle = "in_observation",
                                   tmpName = NULL,
                                   call = parent.frame()) {
  x <- validateX(x, call = call)
  indexDate <- validateIndexDate(indexDate, null = FALSE, x = x, call = call)
  if (!is.list(window)) window <- list(window)
  window <- omopgenerics::validateWindowArgument(window, call = call)
  assertNameStyle(nameStyle = nameStyle, values = list("window_name" = window), call = call)
  newColumns <- glue::glue(nameStyle, window_name = names(window))
  overwriteCols <- newColumns[newColumns %in% colnames(x)]
  if (length(overwriteCols) > 0) {
    cli::cli_warn(c("!" = "{overwriteCols} column{?s} will be overwritten"))
    x <- x |>
      dplyr::select(!dplyr::all_of(overwriteCols))
  }

  cdm <- omopgenerics::cdmReference(x)
  personVariable <- c("person_id", "subject_id")
  personVariable <- personVariable[personVariable %in% colnames(x)]

  id <- uniqueColumnName(cols = c(personVariable, indexDate, newColumns), n = 4)
  start <- id[1]
  end <- id[2]
  startDif <- id[3]
  endDif <- id[4]

  xnew <- x |>
    dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
    dplyr::distinct() |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          !!personVariable := "person_id",
          !!start := "observation_period_start_date",
          !!end := "observation_period_end_date"
        ),
      by = personVariable
    ) |>
    dplyr::filter(
      .data[[indexDate]] <= .data[[end]] && .data[[indexDate]] >= .data[[start]]
    ) %>%
    dplyr::mutate(
      !!startDif := !!CDMConnector::datediff(indexDate, start),
      !!endDif := !!CDMConnector::datediff(indexDate, end)
    )
  if(!is.null(tmpName)){
    xnew <- xnew |> dplyr::compute(name = tmpName, temporary = FALSE)
  }

  qR <- NULL
  for (k in seq_along(window)) {
    win <- window[[k]]
    window_name <- names(window)[k]
    nam <- glue::glue(nameStyle) |> as.character()

    if (all(win == c(0, 0))) {
      nQ <- "1"
    } else {
      lower <- win[1]
      upper <- win[2]

      if (completeInterval == TRUE) {
        if (is.infinite(lower) | is.infinite(upper)) {
          nQ <- "0"
        } else {
          nQ <- "dplyr::if_else(
            .data[['{startDif}']] <= {lower} & .data[['{endDif}']] >= {upper}, 1L, 0L
          )"
        }
      } else {
        if (is.infinite(lower)) {
          if (is.infinite(upper)) {
            nQ <- "1"
          } else {
            nQ <- "dplyr::if_else(.data[['{startDif}']] <= {upper}, 1L, 0L)"
          }
        } else {
          if (is.infinite(upper)) {
            nQ <- "dplyr::if_else({lower} <= .data[['{endDif}']], 1L, 0L)"
          } else {
            nQ <- "dplyr::if_else(
            {lower} <= .data[['{endDif}']] & .data[['{startDif}']] <= {upper}, 1L, 0L
            )"
          }
        }
      }
    }
    nQ <- paste0(nQ) |>
      glue::glue() |>
      rlang::parse_exprs() |>
      rlang::set_names(nam)
    qR <- c(qR, nQ)
  }

  x <- x |>
    dplyr::left_join(
      xnew |> dplyr::mutate(!!!qR) |> dplyr::select(!dplyr::all_of(id)),
      by = c(personVariable, indexDate)
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(newColumns), ~ dplyr::coalesce(as.integer(.x), 0L)
    ))

  return(x)
}

## This function is never called
## Exists to suppress this NOTE:
## Namespace in Imports field not imported from: ‘lifecycle’
lc <- function() {
  lifecycle::badge("experimental")
}
