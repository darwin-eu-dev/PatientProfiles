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

#' It adds a column of counts of desired events per individual to an events
#' table
#'
#' @param x table containing the individuals for which the number of events
#' wants to be counted
#' @param cdm containing the events table
#' @param eventTableName name of the table of the cdm containing the events
#' of interest
#' @param filter condition to apply to the events table for what events to count
#' @param window window to consider events of, from date of reference in table x
#' to date of event at event table
#' @param name name of the column containing the counts per individual
#' @param eventAt date of reference in table x
#' @param eventDate date to consider in event table
#' @param compute whether compute functionality is wanted
#'
#' @return a table with all the individuals in x with a column containing counts
#' of desired events
#' @export
#'
#' @examples
addNumberEvent <- function(x,
                           cdm,
                           eventTableName,
                           filter = NULL,
                           window = c(NA, NA),
                           name = "number_event",
                           eventAt = "cohort_start_date",
                           eventDate = "cohort_start_date",
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
  checkmate::assertCharacter(eventTableName, len = 1,
                             add = errorMessage,
  )
  column2Check <- eventTableName %in% names(cdm)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `eventTableName` is not found in cdm"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)
  errorMessage <- checkmate::makeAssertCollection()
  columnCheck <- ("subject_id" %in% colnames(cdm[[eventTableName]]) ||
                    "person_id" %in% colnames(cdm[[eventTableName]]))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of cdm[[eventTableName]"
    )
  }
  columnCheck2 <- !(all(c("subject_id","person_id") %in% colnames(cdm[[eventTableName]])))
  if (!isTRUE(columnCheck2)) {
    errorMessage$push(
      "- `subject_id` and `person_id` are both columns of cdm[[eventTableName]]"
    )
  }
  checkmate::assert_integerish(window, len = 2, null.ok = TRUE)
  column2Check <- "subject_id" %in% colnames(x)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `subject_id` is not a column of x"
    )
  }
  checkmate::assertList(filter,null.ok = TRUE)
  if(!is.null(filter)) {
    checkmate::assertCharacter(names(filter),
                               add = errorMessage,
    )
    columnsfilterCheck <- names(filter) %in% colnames(cdm[[eventTableName]])
    if (!isTRUE(columnsfilterCheck)) {
      errorMessage$push(
        "- `the variables in filter` are not found in cdm[[eventTableName]]"
      )
    }
  }
  checkmate::assertCharacter(eventDate, len = 1,
                             add = errorMessage,
  )
  column3Check <- eventDate %in% colnames(cdm[[eventTableName]])
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `eventDate` is not found in cdm[[eventTableName]]"
    )
  }
  checkmate::assertCharacter(eventAt, len = 1,
                             add = errorMessage,
  )
  column4Check <- eventAt %in% colnames(x)
  if (!isTRUE(column4Check)) {
    errorMessage$push(
      "- `eventAt` is not found in x"
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

  events <- cdm[[eventTableName]]
  tryCatch({
    if (!is.null(filter)) {
      namesFilter <- names(filter)
      for (k in 1:length(filter)) {
        events <- events %>%
          dplyr::filter_at(
            dplyr::vars(dplyr::all_of(namesFilter[k])),
            dplyr::any_vars(. == !!filter[[k]])
          )
      }
    }
  }, error = function(e){
    message("An error occurred.")
    message(e)
  }, warning = function(w) {
    message("A warning occurred")
    message(w)
  }
)
  if ("person_id" %in% colnames(events)) {
    events <- events %>%
      dplyr::rename("subject_id" = "person_id")
  }
  events <- events %>%
    dplyr::select("subject_id","event_date" = dplyr::all_of(eventDate)) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "event_at" = dplyr::all_of(eventAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(date_dif = !!CDMConnector::datediff("event_at", "event_date"))
  if (!is.na(window[1])) {
    events <- events %>%
      dplyr::filter(.data$date_dif >= !!window[1])
  }
  if (!is.na(window[2])) {
    events <- events %>%
      dplyr::filter(.data$date_dif <= !!window[2])
  }
  events <- events %>%
    dplyr::group_by(.data$subject_id, .data$event_at) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!eventAt := "event_at") %>%
    dplyr::right_join(
      x,
      by = c("subject_id", eventAt)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(name),
      ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
}
