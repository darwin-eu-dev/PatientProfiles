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

#' Classify the variables between 5 types: "numeric", "categorical", "binary",
#' "date", or NA.
#'
#' @param table Tibble
#'
#' @return Tibble with the variables type and classification
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
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
  x <- dplyr::tibble(
    variable = colnames(table),
    variable_type = lapply(table, pillar::type_sum) %>% unlist()
  ) %>%
    dplyr::mutate(variable_classification = assertClassification(
      .data$variable_type, .env$table
    ))
  return(x)
}

#' @noRd
assertClassification <- function(x, tib) {
  lapply(seq_along(x), function(i) {
    if (x[i] == "lgl") {
      return("binary")
    } else if (x[i] %in% c("chr", "fct", "ord")) {
        return("categorical")
    } else if (x[i] %in% c("date", "dttm")) {
      return("date")
    } else if (x[i] == "drtn") {
      return("numeric")
    } else if (x[i] %in% c("int", "dbl", "int64")) {
      lab <- unique(tib[[i]])
      if (length(lab) <= 2 && all(lab %in% c(0, 1))) {
        return("binary")
      } else {
        return("numeric")
      }
    } else {
      return(as.character(NA))
    }
  }) %>%
    unlist()
}

#' Show the available functions for the 4 classifications of data that are
#' supported (numeric, date, binary and categorical)
#'
#' @param variableClassification A choice between: "numeric", "date", "binary"
#' or "categorical".
#'
#' @return A tibble with the available functions for a certain variable
#' classification (or all if NULL)
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' availableFunctions()
#' availableFunctions("numeric")
#' availableFunctions("date")
#' availableFunctions("binary")
#' availableFunctions("categorical")
#' }
#'
#' @export
#'
availableFunctions <- function(variableClassification = NULL) {
  if (is.null(variableClassification)) {
    return(formats)
  } else{
    checkVariableClassification(variableClassification)
    x <- formats %>%
      dplyr::filter(.data$variable_classification == .env$variableClassification) %>%
      dplyr::select(-"variable_classification")
    return(x)
  }
}

#' Detect automatically variables with a certain classification
#'
#' @param table Tibble
#' @param variableClassification Classification of interest, choice between
#' "numeric", "date", "binary" and "categorical"
#' @param exclude Variables to exclude
#'
#' @return Variables in x with the desired classification
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' detectVariables(x, "numeric")
#' }
#'
#' @export
#'
detectVariables <- function(table,
                            variableClassification,
                            exclude = c(
                              "person_id", "subject_id", "cohort_definition_id",
                              "cohort_name", "strata_name", "strata_level"
                            )) {
  # initial checks
  checkTable(table)
  checkVariableClassification(variableClassification)
  checkExclude(exclude)

  # get variable types
  variables <- variableTypes(table) %>%
    dplyr::filter(
      .data$variable_classification == .env$variableClassification
    ) %>%
    dplyr::pull("variable")

  # eliminate excluded variables
  variables <- variables[!(variables %in% exclude)]

  return(variables)
}

#' @noRd
getFunctions <- function(f) {
  estimates_func <- list(
    "min" = function(x) {
      base::min(x, na.rm = TRUE)
    },
    "max" = function(x) {
      base::max(x, na.rm = TRUE)
    },
    "mean" = function(x) {
      base::mean(x, na.rm = TRUE)
    },
    "median" = function(x) {
      stats::median(x, na.rm = TRUE)
    },
    "sum" = function(x) {
      base::sum(x, na.rm = TRUE)
    },
    "iqr" = function(x) {
      stats::IQR(x, na.rm = TRUE)
    },
    "range" = function(x) {
      base::diff(base::range(x, na.rm = TRUE))
    },
    "sd" = function(x) {
      stats::sd(x, na.rm = TRUE)
    },
    "q05" = function(x) {
      stats::quantile(x, 0.05, na.rm = TRUE)
    },
    "q10" = function(x) {
      stats::quantile(x, 0.1, na.rm = TRUE)
    },
    "q15" = function(x) {
      stats::quantile(x, 0.15, na.rm = TRUE)
    },
    "q20" = function(x) {
      stats::quantile(x, 0.2, na.rm = TRUE)
    },
    "q25" = function(x) {
      stats::quantile(x, 0.25, na.rm = TRUE)
    },
    "q30" = function(x) {
      stats::quantile(x, 0.3, na.rm = TRUE)
    },
    "q35" = function(x) {
      stats::quantile(x, 0.35, na.rm = TRUE)
    },
    "q40" = function(x) {
      stats::quantile(x, 0.4, na.rm = TRUE)
    },
    "q45" = function(x) {
      stats::quantile(x, 0.45, na.rm = TRUE)
    },
    "q55" = function(x) {
      stats::quantile(x, 0.55, na.rm = TRUE)
    },
    "q60" = function(x) {
      stats::quantile(x, 0.6, na.rm = TRUE)
    },
    "q65" = function(x) {
      stats::quantile(x, 0.65, na.rm = TRUE)
    },
    "q70" = function(x) {
      stats::quantile(x, 0.7, na.rm = TRUE)
    },
    "q75" = function(x) {
      stats::quantile(x, 0.75, na.rm = TRUE)
    },
    "q80" = function(x) {
      stats::quantile(x, 0.8, na.rm = TRUE)
    },
    "q85" = function(x) {
      stats::quantile(x, 0.85, na.rm = TRUE)
    },
    "q90" = function(x) {
      stats::quantile(x, 0.9, na.rm = TRUE)
    },
    "q95" = function(x) {
      stats::quantile(x, 0.95, na.rm = TRUE)
    }
  )
  return(estimates_func[f])
}
