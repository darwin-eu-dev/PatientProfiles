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
#' @param x x
#' @param ageGroup ...
#' @param name ...
#' @param compute ...
#'
#' @return
#' @export
#'
#' @examples
addAgeGroup <- function(x,
                        ageGroup = NULL,
                        name = "age_group",
                        compute = TRUE) {
  ageGroup <- lapply(ageGroup, function(xx) {
    xx[1] <- ifelse(is.na(xx[1]), 0, xx[1])
    xx[2] <- ifelse(is.na(xx[2]), 150, xx[2])
    nam <- paste0(xx[1], ";", xx[2])
    xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2], !!name := nam)
    return(xx)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(to_join = 1)
  ageGroup <- ageGroup %>%
    dplyr::inner_join(
      dplyr::tibble(
        to_join = 1,
        age = seq(min(ageGroup$age_min), max(ageGroup$age_max))
      ),
      by = "to_join"
    ) %>%
    dplyr::filter(.data$age >= .data$age_min & .data$age <= .data$age_max) %>%
    dplyr::select(dplyr::all_of(c("age", name)))
  x <- x %>%
    dplyr::left_join(ageGroup, by = "age", copy = TRUE)
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
