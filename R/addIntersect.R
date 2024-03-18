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

#' It creates columns to indicate overlap information between two tables
#' `r lifecycle::badge("deprecated")`
#'
#' @param x Table with individuals in the cdm
#' @param tableName name of the cohort that we want to check for overlap
#' @param filterVariable the variable that we are going to use to filter (e.g.
#' cohort_definition_id)
#' @param filterId the value of filterVariable that we are interested in, it can
#' be a vector
#' @param idName the name of each filterId, must have same length than
#' filterId
#' @param value value of interest to add: it can be count, flag, date or time
#' @param window window to consider events of
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param order last or first date to use for date/time calculations
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#' result <- cdm$cohort1 %>%
#'   addIntersect(tableName = "cohort2", value = "date") %>%
#'   dplyr::collect()
#' }
#'
addIntersect <- function(x,
                         tableName,
                         value,
                         filterVariable = NULL,
                         filterId = NULL,
                         idName = NULL,
                         window = list(c(0, Inf)),
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         targetStartDate = startDateColumn(tableName),
                         targetEndDate = endDateColumn(tableName),
                         order = "first",
                         nameStyle = "{value}_{id_name}_{window_name}") {
  lifecycle::deprecate_warn("0.7.0", "addIntersect()")
  .addIntersect(
    x = x, tableName = tableName, value = value,
    filterVariable = filterVariable, filterId = filterId, idName = idName,
    window = window, indexDate = indexDate, censorDate = censorDate,
    targetStartDate = targetStartDate, targetEndDate = targetEndDate,
    order = order, nameStyle = nameStyle
  )
}

.addIntersect <- function(x,
                         tableName,
                         value,
                         filterVariable = NULL,
                         filterId = NULL,
                         idName = NULL,
                         window = list(c(0, Inf)),
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         targetStartDate = startDateColumn(tableName),
                         targetEndDate = endDateColumn(tableName),
                         order = "first",
                         nameStyle = "{value}_{id_name}_{window_name}") {
  if (!is.list(window)) {
    window <- list(window)
  }
  cdm <- omopgenerics::cdmReference(x)
  # initial checks
  personVariable <- checkX(x)
  checkmate::assertCharacter(tableName, len = 1, any.missing = FALSE)
  checkCdm(cdm, tableName)
  personVariableTable <- checkX(cdm[[tableName]])
  extraValue <- checkValue(value, cdm[[tableName]], tableName)
  filterTbl <- checkFilter(filterVariable, filterId, idName, cdm[[tableName]])
  windowTbl <- checkWindow(window)
  checkVariableInX(indexDate, x)
  checkVariableInX(targetStartDate, cdm[[tableName]], FALSE, "targetStartDate")
  checkVariableInX(targetEndDate, cdm[[tableName]], TRUE, "targetEndDate")
  checkmate::assertChoice(order, c("first", "last"))
  checkVariableInX(censorDate, x, TRUE, "censorDate")

  if (!is.null(censorDate)) {
    checkCensorDate(x, censorDate)
  }
  if (!is.null(idName)) {
    idName <- checkSnakeCase(idName)
  }

  tablePrefix <- omopgenerics::tmpPrefix()

  # define overlapTable that contains the events of interest
  overlapTable <- cdm[[tableName]]
  if (!is.null(filterTbl)) {
    overlapTable <- overlapTable %>%
      dplyr::filter(.data[[filterVariable]] %in% .env$filterId)
  } else {
    filterVariable <- "id"
    filterTbl <- dplyr::tibble("id" = 1, "id_name" = "all")
    overlapTable <- dplyr::mutate(overlapTable, "id" = 1)
  }

  values <- list(
    "id_name" = filterTbl$id_name,
    "window_name" = windowTbl$window_name,
    "value" = value
  )
  assertNameStyle(nameStyle, values)
  x <- warnOverwriteColumns(x = x, nameStyle = nameStyle, values = values)

  # columns that will be added
  newCols <- expand.grid(
    value = value,
    id_name = filterTbl$id_name,
    window_name = windowTbl$window_name
  ) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(colnam = as.character(glue::glue(
      nameStyle,
      value = .data$value,
      id_name = .data$id_name,
      window_name = .data$window_name
    ))) %>%
    dplyr::select("colnam", "value") %>%
    dplyr::mutate(colnam = checkSnakeCase(.data$colnam, verbose = F))

  overlapTable <- overlapTable %>%
    dplyr::select(
      !!personVariable := dplyr::all_of(personVariableTable),
      "id" = dplyr::all_of(filterVariable),
      "overlap_start_date" = dplyr::all_of(targetStartDate),
      "overlap_end_date" = dplyr::all_of(targetEndDate %||% targetStartDate),
      dplyr::all_of(extraValue)
    )

  result <- x %>%
    addFutureObservation(
      indexDate = indexDate,
      futureObservationName = "days_to_add"
    ) %>%
    dplyr::mutate("censor_date" = !!CDMConnector::dateadd(
      indexDate, "days_to_add"
    )) %>%
    dplyr::mutate("censor_date" = .data[[censorDate %||% "censor_date"]])

  result <- result %>%
    dplyr::select(
      dplyr::all_of(personVariable),
      "index_date" = dplyr::all_of(indexDate),
      "censor_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlapTable, by = personVariable) %>%
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix),
      temporary = FALSE,
      overwrite = TRUE
    )

  resultCountFlag <- NULL
  resultDateTimeOther <- NULL
  # Start loop for different windows

  for (i in c(1:nrow(windowTbl))) {
    resultW <- result
    if (!is.infinite(windowTbl$upper[i])) {
      resultW <- resultW %>%
        dplyr::mutate(indicator = dplyr::if_else(.data$index_date >= as.Date(!!CDMConnector::dateadd(
          date = "overlap_start_date", number = -windowTbl$upper[i]
        )), 1, 0))
    } else {
      resultW <- resultW %>% dplyr::mutate(indicator = 1)
    }

    resultW <- resultW %>%
      dplyr::mutate(indicator = dplyr::if_else(.data$overlap_start_date > .data$censor_date,
        0, .data$indicator
      ))

    if (!is.infinite(windowTbl$lower[i])) {
      resultW <- resultW %>%
        dplyr::mutate(indicator = dplyr::if_else(.data$index_date > as.Date(!!CDMConnector::dateadd(
          date = "overlap_end_date", number = -windowTbl$lower[i]
        )), 0, .data$indicator))
    }
    resultW <- resultW %>%
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix),
        temporary = FALSE,
        overwrite = TRUE
      )

    # add count or flag
    if ("count" %in% value | "flag" %in% value) {
      resultCF <- resultW %>%
        dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id) %>%
        dplyr::summarise(count = sum(.data$indicator, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(filterTbl, by = "id", copy = TRUE) %>%
        dplyr::select(-"id") %>%
        dplyr::mutate("window_name" = !!tolower(windowTbl$window_name[i]))
      if ("flag" %in% value) {
        resultCF <- resultCF %>% dplyr::mutate(flag = dplyr::if_else(.data$count > 0, 1, 0))
      }
      if (!("count" %in% value)) {
        resultCF <- dplyr::select(resultCF, -"count")
      }

      if (i == 1) {
        resultCountFlag <- resultCF %>%
          dplyr::compute(
            name = omopgenerics::uniqueTableName(tablePrefix),
            temporary = FALSE,
            overwrite = TRUE
          )
      } else {
        resultCountFlag <- resultCountFlag |>
          dplyr::union_all(resultCF) |>
          dplyr::compute(
            name = omopgenerics::uniqueTableName(tablePrefix),
            temporary = FALSE,
            overwrite = TRUE
          )
      }
    }
    # add date, time or other
    if (length(value[!(value %in% c("count", "flag"))]) > 0) {
      resultDTO <- resultW %>%
        dplyr::filter(.data$indicator == 1) %>%
        dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id)
      if (order == "first") {
        resultDTO <- resultDTO %>%
          dplyr::summarise(
            date = min(.data$overlap_start_date, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        resultDTO <- resultDTO %>%
          dplyr::summarise(
            date = max(.data$overlap_start_date, na.rm = TRUE),
            .groups = "drop"
          )
      }
      resultDTO <- resultDTO %>%
        dplyr::right_join(
          resultW %>%
            dplyr::select(dplyr::all_of(c(personVariable, "index_date", "id"))) %>%
            dplyr::distinct(),
          by = c(personVariable, "index_date", "id")
        )
      if ("days" %in% value) {
        resultDTO <- resultDTO %>%
          dplyr::mutate(
            days = !!CDMConnector::datediff("index_date", "date", interval = "day")
          )
      }
      if (length(extraValue) > 0) {
        resultDTO <- resultDTO %>%
          dplyr::left_join(
            resultW %>%
              dplyr::select(
                dplyr::all_of(personVariable), "index_date", "id",
                "date" = "overlap_start_date", dplyr::all_of(extraValue)
              ) %>%
              dplyr::inner_join(
                resultDTO %>%
                  dplyr::select(dplyr::all_of(
                    c(personVariable, "index_date", "id", "date")
                  )),
                by = c(personVariable, "index_date", "id", "date")
              ) %>%
              dplyr::group_by(.data[[personVariable]], .data$index_date, .data$id) %>%
              dplyr::summarise(
                dplyr::across(
                  dplyr::all_of(extraValue), ~ str_flatten(.x, collapse = "; ")
                ),
                .groups = "drop"
              ),
            by = c(personVariable, "index_date", "id")
          )
      }
      resultDTO <- resultDTO %>%
        dplyr::left_join(filterTbl, by = "id", copy = TRUE) %>%
        dplyr::select(-"id") %>%
        dplyr::mutate("window_name" = !!tolower(windowTbl$window_name[i]))
      if (!("date" %in% value)) {
        resultDTO <- dplyr::select(resultDTO, -"date")
      }

      if (i == 1) {
        resultDateTimeOther <- resultDTO %>%
          dplyr::compute(
            name = omopgenerics::uniqueTableName(tablePrefix),
            temporary = FALSE,
            overwrite = TRUE
          )
      } else {
        resultDateTimeOther <- resultDateTimeOther |>
          dplyr::union_all(resultDTO) |>
          dplyr::compute(
            name = omopgenerics::uniqueTableName(tablePrefix),
            temporary = FALSE,
            overwrite = TRUE
          )
      }
    }
  }

  if (any(c("flag", "count") %in% value)) {
    resultCountFlagPivot <- resultCountFlag %>%
      tidyr::pivot_longer(
        dplyr::any_of(c("count", "flag")),
        names_to = "value",
        values_to = "values"
      )  %>%
      tidyr::pivot_wider(
        names_from = c("value", "id_name", "window_name"),
        values_from = "values",
        names_glue = nameStyle,
        values_fill = 0
      ) %>%
    dplyr::rename(!!indexDate := "index_date") %>%
    dplyr::rename_all(tolower)

    newColCountFlag <- colnames(resultCountFlagPivot)
    newColCountFlag <- newColCountFlag[newColCountFlag %in% newCols$colnam]

    x <- x %>%
      dplyr::left_join(
        resultCountFlagPivot,
        by = c(personVariable, indexDate)
      ) %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(newColCountFlag), ~ dplyr::if_else(is.na(.x), 0, .x)
      )) %>%
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix),
        temporary = FALSE,
        overwrite = TRUE
      )
  }

  if (length(value[!(value %in% c("count", "flag"))]) > 0) {
    values <- value[!(value %in% c("count", "flag"))]
    for (val in values) {
      resultDateTimeOtherX <- resultDateTimeOther %>%
        dplyr::select(
          dplyr::all_of(personVariable), "index_date", dplyr::all_of(val),
          "id_name", "window_name"
        ) %>%
        tidyr::pivot_longer(
          dplyr::all_of(val),
          names_to = "value",
          values_to = "values"
        ) %>%
        tidyr::pivot_wider(
          names_from = c("value", "id_name", "window_name"),
          values_from = "values",
          names_glue = nameStyle
        ) %>%
        dplyr::rename(!!indexDate := "index_date") %>%
        dplyr::rename_all(tolower)

      x <- x %>%
        dplyr::left_join(
          resultDateTimeOtherX, by = c(personVariable, indexDate)
        )
    }

    x <- x %>%
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix),
        temporary = FALSE,
        overwrite = TRUE
      )

  }

  # missing columns
  newCols <- newCols %>%
    dplyr::filter(!.data$colnam %in% colnames(x))

  for (val in as.character(unique(newCols$value))) {
    cols <- newCols$colnam[newCols$value == val]
    valk <- switch(
      val,
      flag = 0,
      count = 0,
      days = as.numeric(NA),
      date = as.Date(NA),
      as.character(NA)
    )

    id <- paste0("id_", paste0(sample(letters, 5), collapse = ""))

    newTib <- dplyr::tibble(!!id := 1)
    newTib[, cols] <- valk
    tmpName <- omopgenerics::uniqueTableName(tablePrefix)
    cdm <- omopgenerics::insertTable(cdm = cdm, name = tmpName, table = newTib)

    x <- x |>
      dplyr::mutate(!!id := 1) |>
      dplyr::inner_join(cdm[[tmpName]], by = id) |>
      dplyr::select(!dplyr::all_of(id)) |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix),
        temporary = FALSE,
        overwrite = TRUE
      )
  }

  x <- dplyr::compute(x)

  cdm <- omopgenerics::dropTable(
    cdm = cdm, name = dplyr::starts_with(tablePrefix)
  )

  return(x)
}

#' Get the name of the start date column for a certain table in the cdm
#'
#' @param tableName Name of the table
#'
#' @return Name of the start date column in that table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' startDateColumn("condition_occurrence")
#' }
#'
startDateColumn <- function(tableName) {
  if (tableName %in% namesTable$table_name) {
    return(namesTable$start_date_name[namesTable$table_name == tableName])
  } else {
    return("cohort_start_date")
  }
}

#' Get the name of the end date column for a certain table in the cdm
#'
#' @param tableName Name of the table
#'
#' @return Name of the end date column in that table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' endDateColumn("condition_occurrence")
#' }
#'
endDateColumn <- function(tableName) {
  if (tableName %in% namesTable$table_name) {
    return(namesTable$end_date_name[namesTable$table_name == tableName])
  } else {
    return("cohort_end_date")
  }
}

#' Get the name of the standard concept_id column for a certain table in the cdm
#'
#' @param tableName Name of the table
#'
#' @return Name of the concept_id column in that table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' standardConceptIdColumn("condition_occurrence")
#' }
#'
standardConceptIdColumn <- function(tableName) {
  if (tableName %in% namesTable$table_name) {
    return(namesTable$concept_id_name[namesTable$table_name == tableName])
  } else {
    return("cohort_definition_id")
  }
}

#' Get the name of the source concept_id column for a certain table in the cdm
#'
#' @param tableName Name of the table
#'
#' @return Name of the source_concept_id column in that table
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' sourceConceptIdColumn("condition_occurrence")
#' }
#'
sourceConceptIdColumn <- function(tableName) {
  if (tableName %in% namesTable$table_name) {
    return(namesTable$source_concept_id_name[namesTable$table_name == tableName])
  } else {
    return(as.character(NA))
  }
}
