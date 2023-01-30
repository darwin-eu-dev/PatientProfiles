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

#' It adds columns of user defined events for individual in a cohort table
#'
#' @param x table containing the individual for which the event to be attached as extra columns
#' @param cdm cdm containing the tables
#' @param eventTableName name of the table of the cdm containing the events
#' of interest
#' @param filter condition to apply to the events table for what events to class as event
#' @param window window to consider events of, from date of reference in table x
#' to date of event at event table
#' @param name the name of the column containing the event date of the event.
#' @param eventDate name of the date field to use as date in the event table
#' @param eventAt name of the date field to use as date in table x
#' @param order define which event to include first = include earliest event for each individual in x,
#' last= include last event for each individual in x or all = include all event.
#' @param compute if TRUE compute the output
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(CohortProfiles)
#' cohort1 <- tibble::tibble(
#'cohort_definition_id = c("1", "1", "1"),
#'subject_id = c("1", "2", "3"),
#'cohort_start_date = c(
#'  as.Date("2010-03-03"),
#'  as.Date("2010-03-01"),
#'  as.Date("2010-02-01")
#'),
#'cohort_end_date = c(
#'  as.Date("2015-01-01"),
#'  as.Date("2013-01-01"),
#'  as.Date("2013-01-01")
#')
#')

#'cdm <- mockCohortProfiles(seed = 100, cohort1 = cohort1)
#'
#'
#'
#'
#'addEvent(
#'    cdm$cohort1,
#'    cdm,
#'    "drug_exposure",
#'    window = c(0, 10000),
#'    eventDate = "drug_exposure_start_date",
#'    order = "first"
#'  )
#' }
addEvent <- function(x,
                     cdm,
                     eventTableName,
                     filter = NULL,
                     window = c(NA, NA),
                     name = "event",
                     eventDate = "cohort_start_date",
                     eventAt = "cohort_start_date",
                     order = "first",
                     compute = TRUE) {
  ## check for user inputs
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
  #check if eventTableName in cdm
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
  column5Check <- order %in% c("first","last","all")
  if (!isTRUE(column5Check)) {
    errorMessage$push(
      "- `order` must be one of first,last or all,"
    )
  }
  checkmate::assertCharacter(order, len = 1,
                             add = errorMessage,
  )
  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )
  checkmate::assert_logical(compute, len = 1,
                            add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)




  # define event table from cdm containing the events of interests
  eventTable <- cdm[[eventTableName]]
  # define event of interests from filter

  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      eventTable <- eventTable %>%
        dplyr::filter_at(
          dplyr::vars(dplyr::all_of(namesFilter[k])),
          dplyr::any_vars(. %in% !!filter[[k]])
        )
    }
  }

 # define rename person_id to subject_id
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))

  # filter out event by min and max defined in window
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", eventAt))))
  #define which event to include first, last or all
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "all") {
    xx <- xx %>%
      dbplyr::window_order(.data$event_date) %>%
      dplyr::mutate(nam = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nam = paste0(.env$name, "_", .data$nam)) %>%
      tidyr::pivot_wider(names_from = "nam", values_from = "event_date")
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}
