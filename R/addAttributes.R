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

#' Get attributes from one cohort to another
#'
#' @param newCohort cohort to which to attach the attributes
#' @param oldCohort cohort from which to get the attributes
#'
#' @return new cohort with added attributes from the other given cohort
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockPatientProfiles()
#' attributes(cdm$cohort1)
#' x <- cdm$cohort1 %>%
#'   filter(cohort_definition_id == 1) %>%
#'   computeQuery()
#' attributes(x)
#' x <- addAttributes(x, cdm$cohort1)
#' attributes(cdm$cohort1)
#' }
#'
addAttributes <- function(newCohort,
                          oldCohort) {
  if (!isTRUE(inherits(newCohort, "tbl_dbi"))) {
    cli::cli_abort("{newCohort} is not a valid table")
  }
  if (!isTRUE(inherits(oldCohort, "tbl_dbi"))) {
    cli::cli_abort("{oldCohort} is not a valid table")
  }

  for (at in names(attributes(oldCohort))) {
    if (is.null(attr(newCohort, at))) {
      attr(newCohort, at) <- attr(oldCohort, at)
    }
  }

  class(newCohort) <- class(oldCohort)

  return(newCohort)
}
