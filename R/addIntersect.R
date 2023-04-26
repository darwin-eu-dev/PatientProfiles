#' It creates columns to indicate overlap information between two tables
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
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
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDate date of reference in cohort table, either for end
#' (overlap) or NULL (if incidence)
#' @param order last or first date to use for date/time calculations
#' @param nameStyle naming of the added column or columns, should include
#' required parameters
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return table with added columns with overlap information
#' @noRd
#'
#' @examples
#' \donttest{
#' cohort1 <- dplyr::tibble(
#'   cohort_definition_id = c(1, 1, 1, 1, 1),
#'   subject_id = c(1, 1, 1, 2, 2),
#'   cohort_start_date = as.Date(
#'     c(
#'       "2020-01-01",
#'       "2020-01-15",
#'       "2020-01-20",
#'       "2020-01-01",
#'       "2020-02-01"
#'     )
#'   ),
#'   cohort_end_date = as.Date(
#'     c(
#'       "2020-01-01",
#'       "2020-01-15",
#'       "2020-01-20",
#'       "2020-01-01",
#'       "2020-02-01"
#'     )
#'   )
#' )
#'
#' cohort2 <- dplyr::tibble(
#'   cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
#'   subject_id = c(1, 1, 1, 2, 2, 2, 1),
#'   cohort_start_date = as.Date(
#'     c(
#'       "2020-01-15",
#'       "2020-01-25",
#'       "2020-01-26",
#'       "2020-01-29",
#'       "2020-03-15",
#'       "2020-01-24",
#'       "2020-02-16"
#'     )
#'   ),
#'   cohort_end_date = as.Date(
#'     c(
#'       "2020-01-15",
#'       "2020-01-25",
#'       "2020-01-26",
#'       "2020-01-29",
#'       "2020-03-15",
#'       "2020-01-24",
#'       "2020-02-16"
#'     )
#'   ),
#' )
#'
#' cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)
#'
#' result <- cdm$cohort1 %>%
#'   addIntersect(
#'     cdm = cdm,
#'     tableName = "cohort2", value = "date"
#'   ) %>%
#'   dplyr::collect()
#' }
#'
addIntersect <- function(x,
                         cdm,
                         tableName,
                         value, # must be only one of the four now
                         filterVariable = NULL,
                         filterId = NULL,
                         idName = NULL,
                         window = list(c(0, Inf)), # list
                         indexDate = "cohort_start_date",
                         targetStartDate = "cohort_start_date", # this is targetDate for time/event
                         targetEndDate = "cohort_end_date", # can be NULL (incidence)
                         order = "first",
                         nameStyle = "{value}_{id_name}_{window_name}",
                         tablePrefix = NULL) {
  # initial checks
  person_variable <- checkX(x)
  checkmate::assertCharacter(tableName, len = 1, any.missing = FALSE)
  checkCdm(cdm, tableName)
  person_variable_table <- checkX(cdm[[tableName]])
  extraValue <- checkValue(value, cdm[[tableName]], tableName)
  filterTbl <- checkFilter(filterVariable, filterId, idName, cdm[[tableName]])
  windowTbl <- checkWindow(window)
  checkVariableInX(indexDate, x)
  checkVariableInX(targetStartDate, cdm[[tableName]], FALSE, "targetStartDate")
  checkVariableInX(targetEndDate, cdm[[tableName]], TRUE, "targetEndDate")
  checkmate::assertChoice(order, c("first", "last"))
  checkNameStyle(nameStyle, filterTbl, windowTbl, value)
  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)
  if(!is.null(idName)) {
    checkSnakeCase(idName)
  }

  originalColnames <- colnames(x)

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
  if (is.null(targetEndDate)) {
    overlapTable <- overlapTable %>%
      dplyr::select(
        !!person_variable := dplyr::all_of(person_variable_table),
        "id" = dplyr::all_of(filterVariable),
        "overlap_start_date" = dplyr::all_of(targetStartDate),
        "overlap_end_date" = dplyr::all_of(targetStartDate),
        dplyr::all_of(extraValue)
      )
  } else {
    overlapTable <- overlapTable %>%
      dplyr::select(
        !!person_variable := dplyr::all_of(person_variable_table),
        "id" = dplyr::all_of(filterVariable),
        "overlap_start_date" = dplyr::all_of(targetStartDate),
        "overlap_end_date" = dplyr::all_of(targetEndDate),
        dplyr::all_of(extraValue)
      )
  }

  result <- x %>%
    dplyr::select(
      dplyr::all_of(person_variable),
      "index_date" = dplyr::all_of(indexDate)
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlapTable, by = person_variable)

  if (is.null(tablePrefix)) {
    result <- CDMConnector::computeQuery(result)
  } else {
    result <- CDMConnector::computeQuery(
      result, paste0(tablePrefix, "_join"), FALSE, attr(cdm, "write_schema"), TRUE
    )
  }

  resultCountFlag <- NULL
  resultDateTimeOther <- NULL
  # Start loop for different windows

  for (i in c(1:nrow(windowTbl))) {
    result_w <- result
    if(result %>% dplyr::tally() %>% dplyr::pull() == 0) {
      result_w <- x %>%
        dplyr::select(
          dplyr::all_of(person_variable),
          "index_date" = dplyr::all_of(indexDate)
        ) %>%
        dplyr::distinct() %>%
        dplyr::full_join(overlapTable, by = person_variable) %>%
        dplyr::mutate(indicator = 0)
      if(overlapTable %>% dplyr::tally() %>% dplyr::pull() != 0) {
        result_w <- result_w %>%
          dplyr::filter(!is.na(.data$id))
      }
      } else {
      if (!is.infinite(windowTbl$upper[i])) {
        result_w <- result_w %>%
          dplyr::mutate(indicator = dplyr::if_else(.data$index_date >= as.Date(!!CDMConnector::dateadd(
            date = "overlap_start_date", number = -windowTbl$upper[i]
          )), 1, 0))
      } else {
        result_w <- result_w %>% dplyr::mutate(indicator = 1)
      }

      if (!is.infinite(windowTbl$lower[i])) {
        result_w <- result_w %>%
          dplyr::mutate(indicator = dplyr::if_else(.data$index_date > as.Date(!!CDMConnector::dateadd(
            date = "overlap_end_date", number = -windowTbl$lower[i]
          )), 0, .data$indicator))
      }}
    if (is.null(tablePrefix)) {
      result_w <- CDMConnector::computeQuery(result_w)
    } else {
      result_w <- CDMConnector::computeQuery(
        result_w, paste0(tablePrefix, "_window"), FALSE, attr(cdm, "write_schema"), TRUE
      )
    }
    # add count or flag
    if ("count" %in% value | "flag" %in% value) {
        resultCF <- result_w %>%
          dplyr::group_by(.data[[person_variable]], .data$index_date, .data$id) %>%
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
        if (is.null(tablePrefix)) {
          resultCF <- CDMConnector::computeQuery(resultCF)
        } else {
          resultCF <- CDMConnector::computeQuery(
            resultCF, paste0(tablePrefix, "_count_flag_", i), FALSE,
            attr(cdm, "write_schema"), TRUE
          )
        }
      if (i == 1) {
        resultCountFlag <- resultCF
      } else {
        resultCountFlag <- dplyr::union_all(resultCountFlag, resultCF)
      }
    }
    # add date, time or other
    if (length(value[!(value %in% c("count", "flag"))]) > 0) {
      resultDTO <- result_w %>%
        dplyr::filter(.data$indicator == 1) %>%
        dplyr::group_by(.data[[person_variable]], .data$index_date, .data$id)
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
          result_w %>%
            dplyr::select(dplyr::all_of(c(person_variable, "index_date", "id"))) %>%
            dplyr::distinct(),
          by = c(person_variable, "index_date", "id")
        )
      if ("time" %in% value) {
        resultDTO <- resultDTO %>%
          dplyr::mutate(
            time = !!CDMConnector::datediff("index_date", "date", interval = "day")
          )
      }
      if (length(extraValue) > 0) {
        resultDTO <- resultDTO %>%
          dplyr::left_join(
            result_w %>%
              dplyr::select(
                dplyr::all_of(person_variable), "index_date", "id",
                "date" = "overlap_start_date", dplyr::all_of(extraValue)
              ) %>%
              dplyr::inner_join(
                resultDTO %>%
                  dplyr::select(dplyr::all_of(
                    c(person_variable, "index_date", "id", "date")
                  )),
                by = c(person_variable, "index_date", "id", "date")
              ) %>%
              dplyr::group_by(.data[[person_variable]], .data$index_date, .data$id) %>%
              dplyr::summarise(
                dplyr::across(
                  dplyr::all_of(extraValue), ~ str_flatten(.x, collapse = "; ")
                ),
                .groups = "drop"
              ),
            by = c(person_variable, "index_date", "id")
          )
      }
      resultDTO <- resultDTO %>%
        dplyr::left_join(filterTbl, by = "id", copy = TRUE) %>%
        dplyr::select(-"id") %>%
        dplyr::mutate("window_name" = !!tolower(windowTbl$window_name[i]))
      if (!("date" %in% value)) {
        resultDTO <- dplyr::select(resultDTO, -"date")
      }
      if (is.null(tablePrefix)) {
        resultDTO <- CDMConnector::computeQuery(resultDTO)
      } else {
        resultDTO <- CDMConnector::computeQuery(
          resultDTO, paste0(tablePrefix, "_date_time_", i), FALSE,
          attr(cdm, "write_schema"), TRUE
        )
      }
      if (i == 1) {
        resultDateTimeOther <- resultDTO
      } else {
        resultDateTimeOther <- dplyr::union_all(resultDateTimeOther, resultDTO)
      }
    }
  }

  if (any(c("flag", "count") %in% value)) {
    resultCountFlag <- resultCountFlag %>%
      tidyr::pivot_longer(
        dplyr::any_of(c("count", "flag")),
        names_to = "value",
        values_to = "values"
      ) %>%
      tidyr::pivot_wider(
        names_from = c("value", "id_name", "window_name"),
        values_from = "values",
        names_glue = nameStyle,
        values_fill = 0
      ) %>%
      dplyr::rename(!!indexDate := "index_date") %>%
      dplyr::rename_all(tolower)

    namesToEliminate <- intersect(names(x), names(resultCountFlag))
    namesToEliminate <- namesToEliminate[
      !(namesToEliminate %in% c(person_variable, indexDate))
    ]
    x <- x %>%
      dplyr::select(-dplyr::all_of(namesToEliminate)) %>%
      dplyr::left_join(
        resultCountFlag,
        by = c(person_variable, indexDate)
      )
    currentColnames <- colnames(x)
    x <- x %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(currentColnames[!(currentColnames %in% originalColnames)]),
        ~ dplyr::if_else(is.na(.x), 0, .x)
      ))
    if (is.null(tablePrefix)) {
      x <- CDMConnector::computeQuery(x)
    } else {
      x <- CDMConnector::computeQuery(
        x, paste0(tablePrefix, "_intersect"), FALSE, attr(cdm, "write_schema"),
        TRUE
      )
    }
  }

  if (length(value[!(value %in% c("count", "flag"))]) > 0) {
    values <- value[!(value %in% c("count", "flag"))]
    for (val in values) {
      resultDateTimeOtherX <- resultDateTimeOther %>%
        dplyr::select(
          "subject_id", "index_date", dplyr::all_of(val), "id_name",
          "window_name"
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

      namesToEliminate <- intersect(names(x), names(resultDateTimeOtherX))
      namesToEliminate <- namesToEliminate[
        !(namesToEliminate %in% c(person_variable, indexDate))
      ]

      x <- x %>%
        dplyr::select(-dplyr::all_of(namesToEliminate)) %>%
        dplyr::left_join(resultDateTimeOtherX,
          by = c(person_variable, indexDate)
        )
    }

    if (is.null(tablePrefix)) {
      x <- CDMConnector::computeQuery(x)
    } else {
      x <- CDMConnector::computeQuery(
        x, paste0(tablePrefix, "_intersect"), FALSE, attr(cdm, "write_schema"),
        TRUE
      )
    }
  }


  return(x)
}
