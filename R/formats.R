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

#' Classify the variables between 5 types: "numeric", "categorical", "binary",
#' "date", or NA.
#'
#' @param table Tibble.
#'
#' @return Tibble with the variables type and classification.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' x <- dplyr::tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' variableTypes(x)
#' }
#'
#' @export
#'
variableTypes <- function(table) {
  checkTable(table)
  if (ncol(table) > 0) {
    x <- dplyr::tibble(
      "variable_name" = colnames(table),
      "variable_type" = lapply(colnames(table), function(x) {
        table %>%
          dplyr::select(dplyr::all_of(x)) %>%
          utils::head(1) %>%
          dplyr::pull() %>%
          dplyr::type_sum() |>
          assertClassification()
      }) %>% unlist()
    )
  } else {
    x <- dplyr::tibble(
      "variable_name" = character(),
      "variable_type" = character()
    )
  }
  return(x)
}

#' @noRd
assertClassification <- function(x) {
  switch(x,
    "chr" = "categorical",
    "fct" = "categorical",
    "ord" = "categorical",
    "date" = "date",
    "dttm" = "date",
    "lgl" = "logical",
    "drtn" = "numeric",
    "dbl" = "numeric",
    "int" = "integer",
    "int64" = "integer",
    NA_character_
  )
}

#' Show the available estimates that can be used for the different variable_type
#' supported.
#'
#' @param variableType A set of variable types.
#' @param fullQuantiles Whether to display the exact quantiles that can be
#' computed or only the qXX to summarise all of them.
#'
#' @return A tibble with the available estimates.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' availableEstimates()
#' availableEstimates("numeric")
#' availableEstimates(c("numeric", "categorical"))
#' }
#'
#' @export
#'
availableEstimates <- function(variableType = NULL, fullQuantiles = FALSE) {
  opts <- unique(formats$variable_type)
  if (is.null(variableType)) {
    variableType <- opts
  }
  omopgenerics::assertChoice(variableType, choices = opts, null = TRUE)
  omopgenerics::assertChoice(fullQuantiles, c(TRUE, FALSE))

  if (fullQuantiles) {
    x <- formats |>
      dplyr::select("estimate_name", "estimate_description") |>
      dplyr::filter(.data$estimate_name == "qXX") |>
      dplyr::distinct()
    quantiles <- lapply(c(1:49, 51:99), function(k) {
      x |>
        dplyr::mutate(
          "new_estimate_name" = gsub(
            "XX", stringr::str_pad(k, width = 2, pad = "0"), .data$estimate_name
          ),
          "new_estimate_description" = gsub(
            "XX", stringr::str_pad(k, width = 2, pad = "0"), .data$estimate_description
          )
        )
    }) |>
      dplyr::bind_rows()
    x <- formats |>
      dplyr::left_join(
        quantiles,
        by = c("estimate_name", "estimate_description"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        "estimate_name" = dplyr::if_else(
          is.na(.data$new_estimate_name),
          .data$estimate_name,
          .data$new_estimate_name
        ),
        "estimate_description" = dplyr::if_else(
          is.na(.data$new_estimate_description),
          .data$estimate_description,
          .data$new_estimate_description
        )
      ) |>
      dplyr::select(!c("new_estimate_name", "new_estimate_description"))
  } else {
    x <- formats
  }

  x |> dplyr::filter(.data$variable_type %in% .env$variableType)
}

binaryVariable <- function(x) {
  u <- unique(x)
  if (length(u) <= 3) {
    u <- as.character(u)
    return(all(u %in% c("0", "1", NA_character_)))
  }
  return(FALSE)
}

#' @noRd
getFunctions <- function(f) {
  return(estimatesFunc[f])
}
