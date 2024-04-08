# Copyright 2022 DARWIN EU (C)
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

#' Create a ggplot from the output of summariseCharacteristics.
#' `r lifecycle::badge("deprecated")`
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' results <- summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#'   ),
#'   cohortIntersect = list(
#'     targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'   )
#' )
#'
#' plotCharacteristics(results)
#'
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCharacteristics <- function(data,
                                xAxis = "variable_name",
                                yAxis = "estimate_value",
                                plotStyle = "barplot",
                                facetVarX = NULL,
                                facetVarY = NULL,
                                colorVars = NULL,
                                vertical_x = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertTRUE(plotStyle %in% c("boxplot", "barplot", "density"), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)

  return(
    plotfunction(
      data,
      xAxis,
      yAxis,
      plotStyle = plotStyle,
      facetVarX,
      facetVarY,
      colorVars,
      vertical_x
    )
  )
}
