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

#' It adds a column to a cohort table with the number of days of follow up
#'
#' @param x cohort table in which to add follow up of individuals
#' @param start_date name of the column containing the start dates
#' @param end_date name of the column containing the end dates
#' @param name name of the column to hold the result of the enquiry:
#' number of days of follow up
#' @param tablePrefix Whether resultant table will rename. By default: NULL
#'
#' @return cohort table with the added column with follow up days
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% addFollowUp(cdm)
#' }
#'
addFollowUp <- function(x,
                        start_date = "cohort_start_date",
                        end_date = "cohort_end_date",
                          name = "follow_up",
                        tablePrefix = TRUE) {

  ## check for standard types of user error

  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertCharacter(start_date, len = 1,
                             add = errorMessage,
  )

  column1Check <- start_date %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `start_date` is not a column of x"
    )
  }

  checkmate::assertCharacter(end_date, len = 1,
                             add = errorMessage,
  )

  column1Check <- end_date %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `end_date` is not a column of x"
    )
  }

  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )

  namecolumnCheck <- name %in% colnames(x)
  if (isTRUE(namecolumnCheck)) {
    warning$push(
      "- `name` is already a column of x"
    )
  }

  if (!is.null(tablePrefix)){
    checkmate::assert_logical(tablePrefix, len = 1,
                              add = errorMessage
    )}

  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  name = rlang::enquo(name)

  x <- x %>%
    dplyr::mutate(
      !!name := CDMConnector::datediff(
        start = !!start_date,
        end = !!end_date
    ) + 1
  )

  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)

}
