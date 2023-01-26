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
#' @param x cohort table to which add Sex
#' @param cdm object containing the person table
#' @param name name of the new column to be added
#' @param compute whether compute functionality is desired
#'
#' @return table x with the added column with sex information
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort <- addSex(cdm$cohort,cdm)
#' }
#'
addSex <- function(x,
                   cdm,
                   name = "sex",
                   compute = TRUE) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }

  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  PersonExists <- "person" %in% names(cdm)
  if (!isTRUE(PersonExists)) {
    errorMessage$push(
      "- `person` is not found in cdm"
    )
  }
  PersonCheck <- inherits(cdm$person, "tbl_dbi")
  if (!isTRUE(PersonCheck)) {
    errorMessage$push(
      "- table `person` is not of the right type"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

  column2Check <- "person_id" %in% colnames(cdm$person)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$person"
    )
  }

  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )

  checkmate::assert_logical(compute, len = 1,
                            add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code

  name <- rlang::enquo(name)
  if("subject_id" %in% colnames(x)){
    x <- cdm[["person"]] %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::inner_join(
        x %>% dplyr::select("subject_id") %>% dplyr::distinct(),
        by = c("subject_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("subject_id", !!name) %>%
      dplyr::right_join(x, by = "subject_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  } else {
    x <- cdm[["person"]] %>%
      dplyr::inner_join(
        x %>% dplyr::select("person_id") %>% dplyr::distinct(),
        by = c("person_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("person_id", !!name) %>%
      dplyr::right_join(x, by = "person_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  }
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
