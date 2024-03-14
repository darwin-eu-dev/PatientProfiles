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
  switch (
    x,
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

#' Show the available functions for the 4 classifications of data that are
#' supported (numeric, date, binary and categorical)
#'
#' @param variableType A choice between: "numeric", "date", "binary"
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
#' availableFunctions("integer")
#' availableFunctions("date")
#' availableFunctions("categorical")
#' availableFunctions("logical")
#' }
#'
#' @export
#'
availableFunctions <- function(variableType = NULL) {
  lifecycle::deprecate_warn("0.7.0", what = "availableFunctions()", with = "availableEstimates()")
  availableEstimates(variableType = variableType)
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
availableEstimates <- function(variableType = NULL, fullQuantiles = FALSE){
  opts <- unique(formats$variable_type)
  if (is.null(variableType)) {
    variableType <- opts
  }
  assertChoice(variableType, choices = opts, null = TRUE)
  assertChoice(fullQuantiles, c(TRUE, FALSE))

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
  estimatesFunc <- list(
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
    "sd" = function(x) {
      stats::sd(x, na.rm = TRUE)
    },
    "q01" = function(x) {
      stats::quantile(x, 0.01, na.rm = TRUE)
    },
    "q02" = function(x) {
      stats::quantile(x, 0.02, na.rm = TRUE)
    },
    "q03" = function(x) {
      stats::quantile(x, 0.03, na.rm = TRUE)
    },
    "q04" = function(x) {
      stats::quantile(x, 0.04, na.rm = TRUE)
    },
    "q05" = function(x) {
      stats::quantile(x, 0.05, na.rm = TRUE)
    },
    "q06" = function(x) {
      stats::quantile(x, 0.06, na.rm = TRUE)
    },
    "q07" = function(x) {
      stats::quantile(x, 0.07, na.rm = TRUE)
    },
    "q08" = function(x) {
      stats::quantile(x, 0.08, na.rm = TRUE)
    },
    "q09" = function(x) {
      stats::quantile(x, 0.09, na.rm = TRUE)
    },
    "q10" = function(x) {
      stats::quantile(x, 0.10, na.rm = TRUE)
    },
    "q11" = function(x) {
      stats::quantile(x, 0.11, na.rm = TRUE)
    },
    "q12" = function(x) {
      stats::quantile(x, 0.12, na.rm = TRUE)
    },
    "q13" = function(x) {
      stats::quantile(x, 0.13, na.rm = TRUE)
    },
    "q14" = function(x) {
      stats::quantile(x, 0.14, na.rm = TRUE)
    },
    "q15" = function(x) {
      stats::quantile(x, 0.15, na.rm = TRUE)
    },
    "q16" = function(x) {
      stats::quantile(x, 0.16, na.rm = TRUE)
    },
    "q17" = function(x) {
      stats::quantile(x, 0.17, na.rm = TRUE)
    },
    "q18" = function(x) {
      stats::quantile(x, 0.18, na.rm = TRUE)
    },
    "q19" = function(x) {
      stats::quantile(x, 0.19, na.rm = TRUE)
    },
    "q20" = function(x) {
      stats::quantile(x, 0.20, na.rm = TRUE)
    },
    "q21" = function(x) {
      stats::quantile(x, 0.21, na.rm = TRUE)
    },
    "q22" = function(x) {
      stats::quantile(x, 0.22, na.rm = TRUE)
    },
    "q23" = function(x) {
      stats::quantile(x, 0.23, na.rm = TRUE)
    },
    "q24" = function(x) {
      stats::quantile(x, 0.24, na.rm = TRUE)
    },
    "q25" = function(x) {
      stats::quantile(x, 0.25, na.rm = TRUE)
    },
    "q26" = function(x) {
      stats::quantile(x, 0.26, na.rm = TRUE)
    },
    "q27" = function(x) {
      stats::quantile(x, 0.27, na.rm = TRUE)
    },
    "q28" = function(x) {
      stats::quantile(x, 0.28, na.rm = TRUE)
    },
    "q29" = function(x) {
      stats::quantile(x, 0.29, na.rm = TRUE)
    },
    "q30" = function(x) {
      stats::quantile(x, 0.30, na.rm = TRUE)
    },
    "q31" = function(x) {
      stats::quantile(x, 0.31, na.rm = TRUE)
    },
    "q32" = function(x) {
      stats::quantile(x, 0.32, na.rm = TRUE)
    },
    "q33" = function(x) {
      stats::quantile(x, 0.33, na.rm = TRUE)
    },
    "q34" = function(x) {
      stats::quantile(x, 0.34, na.rm = TRUE)
    },
    "q35" = function(x) {
      stats::quantile(x, 0.35, na.rm = TRUE)
    },
    "q36" = function(x) {
      stats::quantile(x, 0.36, na.rm = TRUE)
    },
    "q37" = function(x) {
      stats::quantile(x, 0.37, na.rm = TRUE)
    },
    "q38" = function(x) {
      stats::quantile(x, 0.38, na.rm = TRUE)
    },
    "q39" = function(x) {
      stats::quantile(x, 0.39, na.rm = TRUE)
    },
    "q40" = function(x) {
      stats::quantile(x, 0.40, na.rm = TRUE)
    },
    "q41" = function(x) {
      stats::quantile(x, 0.41, na.rm = TRUE)
    },
    "q42" = function(x) {
      stats::quantile(x, 0.42, na.rm = TRUE)
    },
    "q43" = function(x) {
      stats::quantile(x, 0.43, na.rm = TRUE)
    },
    "q44" = function(x) {
      stats::quantile(x, 0.44, na.rm = TRUE)
    },
    "q45" = function(x) {
      stats::quantile(x, 0.45, na.rm = TRUE)
    },
    "q46" = function(x) {
      stats::quantile(x, 0.46, na.rm = TRUE)
    },
    "q47" = function(x) {
      stats::quantile(x, 0.47, na.rm = TRUE)
    },
    "q48" = function(x) {
      stats::quantile(x, 0.48, na.rm = TRUE)
    },
    "q49" = function(x) {
      stats::quantile(x, 0.49, na.rm = TRUE)
    },
    "q51" = function(x) {
      stats::quantile(x, 0.51, na.rm = TRUE)
    },
    "q52" = function(x) {
      stats::quantile(x, 0.52, na.rm = TRUE)
    },
    "q53" = function(x) {
      stats::quantile(x, 0.53, na.rm = TRUE)
    },
    "q54" = function(x) {
      stats::quantile(x, 0.54, na.rm = TRUE)
    },
    "q55" = function(x) {
      stats::quantile(x, 0.55, na.rm = TRUE)
    },
    "q56" = function(x) {
      stats::quantile(x, 0.56, na.rm = TRUE)
    },
    "q57" = function(x) {
      stats::quantile(x, 0.57, na.rm = TRUE)
    },
    "q58" = function(x) {
      stats::quantile(x, 0.58, na.rm = TRUE)
    },
    "q59" = function(x) {
      stats::quantile(x, 0.59, na.rm = TRUE)
    },
    "q60" = function(x) {
      stats::quantile(x, 0.60, na.rm = TRUE)
    },
    "q61" = function(x) {
      stats::quantile(x, 0.61, na.rm = TRUE)
    },
    "q62" = function(x) {
      stats::quantile(x, 0.62, na.rm = TRUE)
    },
    "q63" = function(x) {
      stats::quantile(x, 0.63, na.rm = TRUE)
    },
    "q64" = function(x) {
      stats::quantile(x, 0.64, na.rm = TRUE)
    },
    "q65" = function(x) {
      stats::quantile(x, 0.65, na.rm = TRUE)
    },
    "q66" = function(x) {
      stats::quantile(x, 0.66, na.rm = TRUE)
    },
    "q67" = function(x) {
      stats::quantile(x, 0.67, na.rm = TRUE)
    },
    "q68" = function(x) {
      stats::quantile(x, 0.68, na.rm = TRUE)
    },
    "q69" = function(x) {
      stats::quantile(x, 0.69, na.rm = TRUE)
    },
    "q70" = function(x) {
      stats::quantile(x, 0.70, na.rm = TRUE)
    },
    "q71" = function(x) {
      stats::quantile(x, 0.71, na.rm = TRUE)
    },
    "q72" = function(x) {
      stats::quantile(x, 0.72, na.rm = TRUE)
    },
    "q73" = function(x) {
      stats::quantile(x, 0.73, na.rm = TRUE)
    },
    "q74" = function(x) {
      stats::quantile(x, 0.74, na.rm = TRUE)
    },
    "q75" = function(x) {
      stats::quantile(x, 0.75, na.rm = TRUE)
    },
    "q76" = function(x) {
      stats::quantile(x, 0.76, na.rm = TRUE)
    },
    "q77" = function(x) {
      stats::quantile(x, 0.77, na.rm = TRUE)
    },
    "q78" = function(x) {
      stats::quantile(x, 0.78, na.rm = TRUE)
    },
    "q79" = function(x) {
      stats::quantile(x, 0.79, na.rm = TRUE)
    },
    "q80" = function(x) {
      stats::quantile(x, 0.80, na.rm = TRUE)
    },
    "q81" = function(x) {
      stats::quantile(x, 0.81, na.rm = TRUE)
    },
    "q82" = function(x) {
      stats::quantile(x, 0.82, na.rm = TRUE)
    },
    "q83" = function(x) {
      stats::quantile(x, 0.83, na.rm = TRUE)
    },
    "q84" = function(x) {
      stats::quantile(x, 0.84, na.rm = TRUE)
    },
    "q85" = function(x) {
      stats::quantile(x, 0.85, na.rm = TRUE)
    },
    "q86" = function(x) {
      stats::quantile(x, 0.86, na.rm = TRUE)
    },
    "q87" = function(x) {
      stats::quantile(x, 0.87, na.rm = TRUE)
    },
    "q88" = function(x) {
      stats::quantile(x, 0.88, na.rm = TRUE)
    },
    "q89" = function(x) {
      stats::quantile(x, 0.89, na.rm = TRUE)
    },
    "q90" = function(x) {
      stats::quantile(x, 0.90, na.rm = TRUE)
    },
    "q91" = function(x) {
      stats::quantile(x, 0.91, na.rm = TRUE)
    },
    "q92" = function(x) {
      stats::quantile(x, 0.92, na.rm = TRUE)
    },
    "q93" = function(x) {
      stats::quantile(x, 0.93, na.rm = TRUE)
    },
    "q94" = function(x) {
      stats::quantile(x, 0.94, na.rm = TRUE)
    },
    "q95" = function(x) {
      stats::quantile(x, 0.95, na.rm = TRUE)
    },
    "q96" = function(x) {
      stats::quantile(x, 0.96, na.rm = TRUE)
    },
    "q97" = function(x) {
      stats::quantile(x, 0.97, na.rm = TRUE)
    },
    "q98" = function(x) {
      stats::quantile(x, 0.98, na.rm = TRUE)
    },
    "q99" = function(x) {
      stats::quantile(x, 0.99, na.rm = TRUE)
    }
  )
  return(estimatesFunc[f])
}
