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

#' It adds a column to a cohort table indicating whether its individuals are
#' in observation at the desired time
#'
#' @param x cohort table in which the inObservation command wants to be tested
#' @param cdm where the observation_period table is stored
#' @param observationAt name of the column with the dates to test the
#' inObservation command
#' @param name name of the column to hold the result of the enquiry:
#' TRUE if in individual observation, FALSE if not
#' @param compute whether to add compute functionality
#'
#' @return cohort table with the added column assessing inObservation
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort <- inObservation(cdm$cohort,cdm)
#' }
#'
inObservation <- function(x,
                          cdm,
                          observationAt = "cohort_start_date",
                          name = "in_observation",
                          compute = TRUE) {

  ## check for standard types of user error

  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  checkmate::assertTRUE(xCheck, add = errorMessage)
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }

  cdmCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmCheck,
                        add = errorMessage
  )
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  cdmObsPeriodCheck <- inherits(cdm$observation_period, "tbl_dbi")
  checkmate::assertTRUE(cdmObsPeriodCheck, add = errorMessage)
  if (!isTRUE(cdmObsPeriodCheck)) {
    errorMessage$push(
      "- table `observation_period` is not found in cdm"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertCharacter(observationAt,
                             add = errorMessage,
  )

  column1Check <- observationAt %in% colnames(x)
  checkmate::assertTRUE(column1Check,
                        add = errorMessage
  )
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `observationAt` is not a column of x"
    )
  }

  column2Check <- "subject_id" %in% colnames(x)
  checkmate::assertTRUE(column2Check,
                        add = errorMessage
  )
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `subject_id` is not a column of x"
    )
  }

  column3Check <- "person_id" %in% colnames(cdm$observation_period)
  checkmate::assertTRUE(column3Check,
                        add = errorMessage
  )
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$observation_period"
    )
  }

  column4Check <- "observation_period_start_date" %in% colnames(cdm$observation_period)
  checkmate::assertTRUE(column3Check,
                        add = errorMessage
  )
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `observation_period_start_date` is not a column of cdm$observation_period"
    )
  }

  column5Check <- "observation_period_end_date" %in% colnames(cdm$observation_period)
  checkmate::assertTRUE(column5Check,
                        add = errorMessage
  )
  if (!isTRUE(column5Check)) {
    errorMessage$push(
      "- `observation_period_end_date` is not a column of cdm$observation_period"
    )
  }

  checkmate::assertCharacter(name,
                             add = errorMessage,
  )

  checkmate::assert_logical(compute,
                            add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  name = rlang::enquo(name)

    x <- x %>%
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
        !!name := dplyr::if_else(
          .data[[observationAt]] >= .data$observation_period_start_date &
            .data[[observationAt]] <= .data$observation_period_end_date,
          TRUE,
          FALSE
        )
      ) %>%
      dplyr::select(
        -"observation_period_start_date", - "observation_period_end_date"
      )

    if (isTRUE(compute)) {
      x <- x %>% dplyr::compute()
    }
    return(x)

}
