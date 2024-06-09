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
  ageGroup <- validateAgeGroup(ageGroup, call = call)
  notIndexDate <- !any(c(
    age, !is.null(ageGroup), priorObservation, futureObservation
  ))
  indexDate <- validateIndexDate(indexDate, null = notIndexDate, x = x, call = call)
  ageName <- validateColumn(ageName, null = !age,  call = call)
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
        pHQ <- '.data$start_date'
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
        fOQ <- '.data$end_date'
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
        sQ <- glue::glue('{sQ}, .default = NA_character_)')
      } else {
        sQ <- glue::glue('{sQ}, .default = "{missingSexValue}")')
      }
      sQ <- sQ |> rlang::parse_exprs() |> rlang::set_names(sexName)
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
      xx <- c(xx, '.default = NA_character_')
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
