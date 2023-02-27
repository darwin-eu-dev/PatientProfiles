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

#' It adds all demographics columns to the given cohort table: Age, Sex,
#' PriorHistory, and ageGroup if desired
#'
#' @param x cohort table in which to add follow up of individuals
#' @param cdm cdm with the person and observation_period tables to get the info
#' for the individuals in the cohort
#' @param demographicsAt name of the column with the date at which consider
#' demographic information
#' @param ageGroup if not NULL, a list of ageGroup vectors
#' @param compute whether to add compute functionality
#'
#' @return cohort table with the added demographic information columns
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% addDemographics(cdm)
#' }
#'
addDemographics <- function(x,
                        cdm,
                        demographicsAt = "cohort_start_date",
                        ageGroup = NULL,
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

  checkmate::assertCharacter(demographicsAt, len = 1,
                             add = errorMessage,
  )

  column1Check <- demographicsAt %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `demographicsAt` is not a column of x"
    )
  }

  # check for ageGroup, change it to a list if it is a vector of length 2, push error if other length
  if (isTRUE(checkmate::checkIntegerish(ageGroup))) {
    if (length(ageGroup) == 2) {
      ageGroup <- list(ageGroup)
    } else {
      errorMessage$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }
  }

  # after changing vector to list, we check it is a list with numeric
  checkmate::assertList(ageGroup,
                        types = "integerish", null.ok = TRUE,
                        add = errorMessage
  )

  # each vector in the list has to have length 2, push error if not
  if (!is.null(ageGroup)) {
    lengthsAgeGroup <- checkmate::assertTRUE(unique(lengths(ageGroup)) == 2,
                                             add = errorMessage
    )
    if (!isTRUE(lengthsAgeGroup)) {
      errorMessage$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }

    # first sort ageGroup by first value
    ageGroup <- ageGroup[order(sapply(ageGroup, function(x) x[1], simplify = TRUE), decreasing = FALSE)]

    # check lower bound is smaller than upper bound, allow NA in ageGroup at the moment
    checkAgeGroup <- unlist(lapply(ageGroup, function(x) {
      x[1] <= x[2]
    }))
    checkmate::assertTRUE(all(checkAgeGroup, na.rm = TRUE),
                          add = errorMessage
    )

    # check ageGroup overlap
    list1 <- lapply(dplyr::lag(ageGroup), function(x) {
      x[2]
    })
    list1 <- unlist(list1[lengths(list1) != 0])
    list2 <- lapply(dplyr::lead(ageGroup), function(x) {
      x[1]
    })
    list2 <- unlist(list2[lengths(list2) != 0])

    # the first value of the interval needs to be larger than the second value of previous vector
    checkOverlap <- checkmate::assertTRUE(all(list2 - list1 > 0), add = errorMessage)
    if (!isTRUE(checkOverlap)) {
      errorMessage$push("- ageGroup can not have overlapping intervals")
    }
  }

  checkmate::assert_logical(compute, len = 1,
                            add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  x <- x %>%
    addAge(cdm, ageAt = demographicsAt) %>%
    addSex(cdm) %>%
    addPriorHistory(cdm, priorHistoryAt = demographicsAt)
  if (!is.null(ageGroup)) {
    x <- x %>%
      addAgeGroup(cdm, ageGroup = ageGroup)
  }
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)

}
