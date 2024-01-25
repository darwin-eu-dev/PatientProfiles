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

#' A tidy implementation of the summarised_characteristics object.
#'
#' @param result A summarised_characteristics object.
#' @param minCellCount Minimum number of counts to report.
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 |>
#'   summariseCharacteristics() |>
#'   tidyCharacteristics()
#' }
#'
#' @return A tibble with a tidy version of the summarised_characteristics
#' object.
#'
#' @export
#'
tidyCharacteristics <- function(result, minCellCount = 5) {
  result <- omopgenerics::summarisedResult(result)
  if (!inherits(result, "summarised_characteristics")) {
    cli::cli_abort("result is not a valid `summarised_characteristics` object.")
  }
  result |>
    omopgenerics::suppress(minCellCount = minCellCount) |>
    dplyr::select(-c("result_type", "package_name", "package_version")) |>
    visOmopResults::splitAll()
}
