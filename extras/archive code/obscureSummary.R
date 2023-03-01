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

#' This function is used to obscure small counts in population summary tables
#' @param result table with the population summary, must have variables
#' cohort_definition_id, variable, estimate and value
#' @param minimumCellCounts number below which the cell results are obscured
#' @param globalVariables variables for which whole cohorts are obscured if
#' their estimatesToObscure are below the minimumCellCount
#' @param estimatesToObscure estimates to obscure if below minimumCellCount
#'
#' @return table with the required cells obscured
#' @export
#'
#' @examples
#'\dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' summary_c1 <- getTableOne(cdm,cohort1)
#' summary_obscured <- obscureSummary(summary_c1)
#' }
#'
#'
obscureSummary <- function(result,minimumCellCounts = 5,globalVariables =
                             c("number_observations", "number_subjects"),
                           estimatesToObscure = "count") {

  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  column1Check <- c("cohort_definition_id") %in% colnames(result)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `cohort_definition_id` is not a column of result"
    )
  }
  column2Check <- c("variable") %in% colnames(result)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `variable` is not a column of result"
    )
  }
  column3Check <- c("estimate") %in% colnames(result)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `estimate` is not a column of result"
    )
  }
  column4Check <- c("value") %in% colnames(result)
  if (!isTRUE(column4Check)) {
    errorMessage$push(
      "- `value` is not a column of result"
    )
  }
  checkmate::assertIntegerish(minimumCellCounts, len = 1,
                             add = errorMessage,
  )
  checkmate::assertCharacter(globalVariables,
                             add = errorMessage,
  )
  checkmate::assertCharacter(estimatesToObscure,
                             add = errorMessage,
  )
  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  values_to_osbcure <- suppressWarnings(as.numeric(result$value)) <
    minimumCellCounts &
    suppressWarnings(as.numeric(result$value)) > 0
  obscured_values <- result$estimate %in% estimatesToObscure & values_to_osbcure
  obscured_cohort <- unique(result$cohort_definition_id[
    result$estimate %in% estimatesToObscure &
      result$variable %in% globalVariables &
      values_to_osbcure
  ])
  result$value[obscured_values] <- paste0("<", minimumCellCounts)
  result$value[
    result$cohort_definition_id %in% obscured_cohort
  ] <- as.character(NA)
  result$value[
    result$cohort_definition_id %in% obscured_cohort &
      result$variable %in% globalVariables
  ] <- paste0("<", minimumCellCounts)
  return(result)
}
