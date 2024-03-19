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
#' plot characteristics
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data.
#' @param plotStyle boxplot or barplot.
#' @param facetVars column in data to facet by.
#' @param colorVars column in data to color by.
#' @param facetOrder order of facet, make  sure multiple facets are separated by period and in the order provided in facetVars.
#' @param colorNames A vector or pre-selected color.
#' @param vertical_x whether to display x axis string vertically.
#' @param options Other plot options in a list.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' results <- summariseCharacteristics(
#'  cohort = cdm$cohort1,
#'  ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'  tableIntersect = list(
#'  tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#' ),
#' cohortIntersect = list(
#'  targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'  )
#' )
#' graph <- plotCharacteristics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCharacteristics <- function(data,
                                xAxis = "variable_name",
                                yAxis = "estimate_value",
                                plotStyle = "barplot",
                                facetVars = NULL,
                                colorVars = NULL,
                                facetOrder = NULL,
                                colorNames = NULL,
                                vertical_x = FALSE,
                                options = list()) {
  return(plotfunction(
    data,
    xAxis,
    yAxis,
    plotStyle,
    facetVars,
    colorVars,
    facetOrder,
    colorNames,
    vertical_x,
    options
  ))
}
