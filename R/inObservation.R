# Copyright 2022 DARWIN EU (C)
#
# This file is part of CohortProfiles
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

#' It creates a mock database for testing CohortProfiles package
#'
#' @param x ...
#' @param cdm ...
#' @param compute ...
#'
#' @return
#' @export
#'
#' @examples
inObservation <- function(x,
                          cdm,
                          compute = TRUE) {
  x %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::mutate(
      in_observation = dplyr::if_else(
        .data$cohort_start_date >= .data$observation_period_start_date &
          .data$cohort_start_date <= .data$observation_period_end_date,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(
      -"observation_period_start_date", - "observation_period_end_date"
    ) %>%
    dplyr::compute()
}
