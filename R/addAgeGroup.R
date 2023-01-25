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
#' @param x Tibble with the individuals want to get age group, need to be from a cdm object if does not have "age" column
#' @param ageGroup a list of ageGroup vectors. e.g. list(c(0,10),c(11,20)). Default: 0, 150
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a cdm reference.
#' @param ageGroupNames a vector of character. names for age groups, if not provided, default combines characters in ageGroup. e.g. 0;150 if no ageGroup and ageGroupNames provided
#' @param compute Whether resultant table will be computed as temporal table. By default: TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' #'
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(CohortProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
#'   )
#' )
#'
#' person <- tibble::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8507", "8507"),
#'   year_of_birth = c(1980, 1970, 2000),
#'   month_of_birth = c(03, 07, NA),
#'   day_of_birth = c(NA, 02, 01)
#' )
#'
#' cdm <- mockDrugUtilisation(person = person, cohort1 = cohort1)
#'
#' result <- addAgeGroup(
#'   x = cdm$cohort1, ageGroup = list(c(1, 20), c(21, 30), c(31, 40)), cdm = cdm
#' )
#' }
addAgeGroup <- function(x,
                        ageGroup = NULL,
                        cdm = NULL,
                        ageGroupNames = NULL,
                        compute = TRUE) {
  messageStore <- checkmate::makeAssertCollection()


  # if age column not found in x, we need cdm object and addAge function to impute age

  if (!isTRUE("age" %in% colnames(x))) {
    checkmate::assertClass(cdm, classes = "cdm_reference", add = messageStore)
    print("- age column not found, will be imputed by addAge function")
    x <- addAge(x, cdm)
  }


  # check for ageGroup, change it to a list if it is a vector of length 2, push error if other length
  if (is.numeric(ageGroup)) {
    if (length(ageGroup) == 2) {
      ageGroup <- list(ageGroup)
    } else {
      messageStore$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }
  }

  # after changing vector to list, we check it is a list with numeric
  checkmate::assertList(ageGroup,
    types = "numeric", null.ok = TRUE,
    add = messageStore
  )

  # each vector in the list has to have length 2, push error if not
  if (!is.null(ageGroup)) {
    lengthsAgeGroup <- checkmate::assertTRUE(unique(lengths(ageGroup)) == 2,
      add = messageStore
    )
    if (!isTRUE(lengthsAgeGroup)) {
      messageStore$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }
    # check lower bound is smaller than upper bound, allow NA in ageGroup at the moment
    checkAgeGroup <- unlist(lapply(ageGroup, function(x) {
      x[1] <= x[2]
    }))
    checkmate::assertTRUE(all(checkAgeGroup, na.rm = TRUE),
      add = messageStore
    )
  }


  if (is.null(ageGroup)) {
    ageGroup <- list(c(NA, NA))
  }


  # if ageGroupNames is provided, it needs to be the same length as ageGroup
  checkmate::assertCharacter(ageGroupNames,
    len = length(ageGroup),
    null.ok = TRUE, add = messageStore
  )

  checkmate::reportAssertions(collection = messageStore)


  # if no ageGroup provided, use 0;150 as ageGroup and ageGroupName
  # if provided ageGroup, but no ageGroupNames provided:
  # ageGroupName will be combination of each range vector
  ageGroup <- lapply(ageGroup, function(xx) {
    xx[1] <- ifelse(is.na(xx[1]), 0, xx[1])
    xx[2] <- ifelse(is.na(xx[2]), 150, xx[2])
    if (is.null(ageGroupNames)) {
      nam <- paste0(xx[1], ";", xx[2])
      xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2], ageGroupNames := nam)
    } else {
      xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2])
    }
    return(xx)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(to_join = 1)

  if (!is.null(ageGroupNames)) {
    ageGroup <- ageGroup %>% dplyr::mutate(ageGroupNames = ageGroupNames)
  }


  # people with NA age will be classified as NA group in ageGroupNames column
  ageGroup <- ageGroup %>%
    dplyr::inner_join(
      dplyr::tibble(
        to_join = 1,
        age = seq(min(ageGroup$age_min), max(ageGroup$age_max))
      ),
      by = "to_join"
    ) %>%
    dplyr::filter(.data$age >= .data$age_min & .data$age <= .data$age_max) %>%
    dplyr::select("age", ageGroupNames)
  x <- x %>%
    dplyr::left_join(ageGroup, by = "age", copy = TRUE)
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
