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

#' Add the ordinal number of the observation period associated that a given date
#' is in.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param nameObservationPeriodId Name of the new colum.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return Table with the current observation period id added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addObservationPeriodId()
#' mockDisconnect(cdm = cdm)
#' }
#'
addObservationPeriodId <- function(x,
                                   indexDate = "cohort_start_date",
                                   nameObservationPeriodId = "observation_period_id",
                                   name = NULL) {
  x <- validateX(x)
  name <- omopgenerics::validateNameArgument(
    name = name,
    cdm = omopgenerics::cdmReference(x),
    validation = "warning",
    null = TRUE
  )
  x |>
    .addObservationPeriodIdQuery(
      indexDate = indexDate,
      nameObservationPeriodId = nameObservationPeriodId
    ) |>
    computeTable(name = name)
}

#' Add the ordinal number of the observation period associated that a given date
#' is in. Result is not computed, only query is added.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param nameObservationPeriodId Name of the new colum.
#'
#' @return Table with the current observation period id added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addObservationPeriodIdQuery()
#' mockDisconnect(cdm = cdm)
#' }
#'
addObservationPeriodIdQuery <- function(x,
                                        indexDate = "cohort_start_date",
                                        nameObservationPeriodId = "observation_period_id") {
  x |>
    validateX() |>
    .addObservationPeriodIdQuery(
      indexDate = indexDate,
      nameObservationPeriodId = nameObservationPeriodId
    )
}

.addObservationPeriodIdQuery <- function(x,
                                         indexDate,
                                         nameObservationPeriodId,
                                         call = parent.frame()){
  cdm <- omopgenerics::cdmReference(x)
  indexDate <- validateIndexDate(indexDate, null = FALSE, x = x, call = call)
  personVariable <- c("person_id", "subject_id")
  personVariable <- personVariable[personVariable %in% colnames(x)]
  nameObservationPeriodId <- validateColumn(nameObservationPeriodId, call = call)

  # drop variable if it already exists
  if(nameObservationPeriodId %in% colnames(x)){
    cli::cli_warn(c("!" = "Existing {nameObservationPeriodId} column will be overwritten"))
    x <- x |>
      dplyr::select(!dplyr::all_of(nameObservationPeriodId))
  }

  # if empty table, return with variable name added
  if(x |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") == 0){
    return(x |>
             dplyr::mutate(!!nameObservationPeriodId := as.integer(NA)))
  }

  cols <- omopgenerics::uniqueId(n = 2, exclude = colnames(x))

  currentObsId <- x |>
    dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          !!personVariable :="person_id",
          !!cols[1] := "observation_period_start_date",
          !!cols[2] := "observation_period_end_date"
        ),
      by = personVariable
    ) |>
    dplyr::group_by(.data[[personVariable]], .data[[indexDate]]) |>
    dplyr::arrange(.data[[cols[1]]]) |>
    dplyr::mutate(!!nameObservationPeriodId := as.integer(dplyr::row_number())) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data[[indexDate]] <= .data[[cols[2]]] &&
        .data[[indexDate]] >= .data[[cols[1]]]
    ) |>
    dplyr::select(dplyr::all_of(c(
      personVariable, indexDate, nameObservationPeriodId
    )))

  x |>
    dplyr::left_join(currentObsId, by = c(personVariable, indexDate))
}
