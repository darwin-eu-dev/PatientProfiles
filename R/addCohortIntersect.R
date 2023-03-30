
#' It creates columns to indicate overlaps information
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param cohortTableName name of the cohort that we want to check for overlap
#' @param cohortId vector of cohort definition ids to include
#' @param value value of interest to add: it can be number, binary, date or time
#' @param window window to consider events of
#' @param indexDate date of reference in table x
#' @param targetStartDate date of reference in cohort table, either for start
#' (in overlap) or on its own (for incidence)
#' @param targetEndDare date of reference in cohort table, either for end
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
#'
#'\dontrun{
#'   cohort1 <- dplyr::tibble(
#'cohort_definition_id = c(1, 1, 1, 1, 1),
#'subject_id = c(1, 1, 1, 2, 2),
#'cohort_start_date = as.Date(
#'  c(
#'    "2020-01-01",
#'    "2020-01-15",
#'    "2020-01-20",
#'    "2020-01-01",
#'    "2020-02-01"
#'  )
#'),
#'cohort_end_date = as.Date(
#'  c(
#'    "2020-01-01",
#'    "2020-01-15",
#'    "2020-01-20",
#'    "2020-01-01",
#'    "2020-02-01"
#'  )
#')
#')
#'
#'cohort2 <- dplyr::tibble(
#'  cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
#'  subject_id = c(1, 1, 1, 2, 2, 2, 1),
#'  cohort_start_date = as.Date(
#'    c(
#'      "2020-01-15",
#'      "2020-01-25",
#'      "2020-01-26",
#'      "2020-01-29",
#'      "2020-03-15",
#'      "2020-01-24",
#'      "2020-02-16"
#'    )
#'  ),
#'  cohort_end_date = as.Date(
#'    c(
#'      "2020-01-15",
#'      "2020-01-25",
#'      "2020-01-26",
#'      "2020-01-29",
#'      "2020-03-15",
#'      "2020-01-24",
#'      "2020-02-16"
#'    )
#'  ),
#')
#'
#'cdm <- mockCohortProfiles(cohort1=cohort1, cohort2=cohort2)
#'
#'result <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
#'cohortTableName = "cohort2", value = "date") %>% dplyr::collect()
#'}
#'
addCohortIntersect <- function(x,
                               cdm,
                               cohortTableName,
                               cohortId = NULL,
                               value = NULL, # must be only one of the four now
                               window = list(c(0, Inf)), #list
                               indexDate = "cohort_start_date",
                               targetStartDate = "cohort_start_date", # this is targetDate for time/event
                               targetEndDate = "cohort_end_date", # can be NULL (incidence)
                               order = "first",
                               nameStyle = "{cohortName}_{window}",
                               tablePrefix = NULL) {
  ## check for user inputs
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push("- x is not a table")
  }
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push("- cdm must be a CDMConnector CDM reference object")
  }
  checkmate::reportAssertions(collection = errorMessage)

  ## check for user inputs
  errorMessage <- checkmate::makeAssertCollection()

  tableCheck <- cohortTableName %in% names(cdm)
  if (!isTRUE(tableCheck)) {
    errorMessage$push("- `cohortTableName` is not found in cdm")
  }
  checkmate::assert_integerish(cohortId, null.ok = TRUE)

  # window <- checkWindow(window) # Update when available

  checkmate::assert_character(value, len = 1)
  valueCheck <-
    value %>% dplyr::intersect(c("number", "binary", "date", "time")) %>% dplyr::setequal(value)
  if (!isTRUE(valueCheck)) {
    errorMessage$push("- `value` must be either 'count','binary','date' or 'time' ")
  }

  orderCheck <- order %in% c("first", "last")
  if (!isTRUE(orderCheck)) {
    errorMessage$push("- `order` must be either 'first' or 'last' ")
  }

  checkmate::assert_character(indexDate, len = 1)
  checkmate::assert_character(targetStartDate, len = 1)
  checkmate::assert_character(targetEndDate, len = 1, null.ok = TRUE)

  columnCheck <- targetStartDate %in% colnames(cdm[[cohortTableName]])
  if (!isTRUE(columnCheck)) {
    errorMessage$push("- `targetStartdate` is not a column of cdm[[cohortTableName]]")
  }

  column2Check <- indexDate %in% colnames(x)
  if (!isTRUE(column2Check)) {
    errorMessage$push("- `indexDate` is not a column of x")
  }

  if(!is.null(targetEndDate)) {
    column3Check <- targetEndDate %in% colnames(cdm[[cohortTableName]])
    if (!isTRUE(column3Check)) {
      errorMessage$push("- `targetEndDate` is not a column of cdm[[cohortTableName]]")
    }
  }

  cohortName <- CDMConnector::cohortSet(cdm[[cohortTableName]])
  if(!is.null(cohortName)) {
    cohortName <- cohortName %>% dplyr::rename(cohortName = cohort_name)
  }
  # If we cannot get the cohort names, let them be the cohort ids
  if(is.null(cohortName) && !is.null(cohortId)) {
    cohortName <- tibble::tibble(cohort_definition_id = cohortId, cohortName = paste0("cohort",cohortId))
  }
  # If there are no cohorts ids, let the cohort names be "all"
  if(is.null(cohortName)) {
    cohortName <- tibble::tibble(cohort_definition_id = cdm[[cohortTableName]] %>%
                                   dplyr::select(cohort_definition_id) %>%
                                   dplyr::distinct() %>% dplyr::pull(),
                                 cohortName = "all")
  }

  parameters <- list("cohortName" = cohortName, "window" = window)
  checkName(nameStyle, parameters)

  nameStyle <- sub("window", "window_name", nameStyle)

  windowNames <- getWindowNames(window)

    checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # define overlapcohort table from cdm containing the events of interests
  overlapCohort <- cdm[[cohortTableName]]

  #generate overlappingcohort using code from getoverlappingcohort
  #filter by cohortId
  if (!is.null(cohortId) && !is.null(targetEndDate)) {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = targetStartDate,
        "overlap_end_date" = targetEndDate,
        "overlap_id" = "cohort_definition_id"
      ) %>% dplyr::filter(.data$overlap_id %in% .env$cohortId) %>%
      CDMConnector::computeQuery()
  } else if(!is.null(cohortId) && is.null(targetEndDate)) {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = targetStartDate,
        "overlap_id" = "cohort_definition_id"
      ) %>% dplyr::filter(.data$overlap_id %in% .env$cohortId) %>%
      dplyr::mutate(overlap_end_date = overlap_start_date) %>%
      dplyr::select(-"cohort_end_date") %>%
      CDMConnector::computeQuery()
  } else if (is.null(cohortId) && !is.null(targetEndDate)){
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = targetStartDate,
        "overlap_end_date" = targetEndDate,
        "overlap_id" = "cohort_definition_id"
      ) %>%
      CDMConnector::computeQuery()
  } else {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = targetStartDate,
        "overlap_id" = "cohort_definition_id"
      ) %>%
      dplyr::mutate(overlap_end_date = overlap_start_date) %>%
      dplyr::select(-"cohort_end_date") %>%
      CDMConnector::computeQuery()
  }

  if("person_id" %in% colnames(x)) {
    x <- x %>%
      dplyr::rename("subject_id" = "person_id") %>%
      CDMConnector::computeQuery()
  }

  result_cb <- x %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id") %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()

  result_dt <- x %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id") %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()

  for (i in c(1:length(window))) {
    workingWindow <- window[[i]]
    window_name <- windowNames[[i]]

      result_w <- x %>%
      dplyr::select("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::inner_join(overlapCohort, by = "subject_id") %>%
      CDMConnector::computeQuery()

      indexDate <- as.character(indexDate)

      if (workingWindow[2] != Inf) {
        result_w <- result_w %>%
          dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
            CDMConnector::dateadd(date = "overlap_start_date",
                                  number = !!-workingWindow[2])
          ))) %>%
          dplyr::filter(!!rlang::sym(indexDate) >= .data$overlap_start_date) %>%
          CDMConnector::computeQuery()
      }
      if (workingWindow[1] != -Inf) {
        result_w <- result_w %>%
          dplyr::mutate(overlap_end_date = as.Date(dbplyr::sql(
            CDMConnector::dateadd(date = "overlap_end_date",
                                  number = !!-workingWindow[1])
          ))) %>%
          dplyr::filter(!!rlang::sym(indexDate) <= .data$overlap_end_date) %>%
          CDMConnector::computeQuery()
      }

    # add count or binary
    if ("number" %in% value | "binary" %in% value) {
      result_cb_w <- result_w %>%
        dplyr::select("subject_id",
                      "cohort_start_date",
                      "cohort_definition_id",
                      "cohort_end_date",
                      "overlap_id") %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date, .data$overlap_id) %>%
        dplyr::summarise(number = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          binary = 1,
          overlap_id = as.numeric(.data$overlap_id),
          window_name = window_name
        ) %>%
        dplyr::left_join(cohortName, by = c("overlap_id" = "cohort_definition_id"), copy = TRUE) %>%
        dplyr::select(-"overlap_id") %>%
        tidyr::pivot_wider(
          names_from = c("cohortName", "window_name"),
          values_from = c("number", "binary"),
          names_glue = paste0("{.value}_",nameStyle),
          values_fill = 0
        )  %>%
        dplyr::right_join(x %>% dplyr::select(c("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id")),
                          by = c("subject_id", "cohort_start_date", "cohort_end_date")) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with(c(
          "binary", "number"
        )),
        ~ dplyr::if_else(is.na(.x), 0, .x))) %>%
        CDMConnector::computeQuery()

    } else {
      result_cb_w <- result_cb
    }


    # add date or time
    if ("date" %in% value | "time" %in% value) {

      workingWindow <- ifelse(workingWindow == Inf,0,workingWindow)
      workingWindow <- ifelse(workingWindow == -Inf,0,workingWindow)

      result_dt_w <- result_w %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date",
          "cohort_definition_id",
          "overlap_id",
          "overlap_start_date"
        ) %>% dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!+workingWindow[2])
        ))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$subject_id, .data$overlap_id, .data$cohort_start_date, .data$cohort_end_date) %>%
        dplyr::mutate(
          min_date = min(.data$overlap_start_date),
          max_date = max(.data$overlap_start_date)
        ) %>%
        dplyr::mutate(
          min_time = !!CDMConnector::datediff("cohort_start_date", "min_date", interval = "day"),
          max_time = !!CDMConnector::datediff("cohort_start_date", "max_date", interval = "day")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"overlap_start_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          overlap_id = as.numeric(.data$overlap_id),
          window_name = window_name
        ) %>%
        dplyr::left_join(cohortName, by = c("overlap_id" = "cohort_definition_id"), copy = TRUE) %>%
        dplyr::select(-"overlap_id") %>%
        tidyr::pivot_wider(
          names_from = c("cohortName", "window_name"),
          values_from = c("min_time", "min_date", "max_time", "max_date"),
          names_glue = paste0("{.value}_",nameStyle),
          values_fill = NA
        )  %>%
        CDMConnector::computeQuery()


      if (order == "first") {
        result_dt_w <-
          result_dt_w  %>%
          dplyr::select(-dplyr::starts_with("max_")) %>%
          dplyr::rename_with( ~ stringr::str_remove_all(., "min_"), dplyr::contains("min_")) %>%
          CDMConnector::computeQuery()
      } else {
        result_dt_w <-
          result_dt_w  %>%
          dplyr::select(-dplyr::starts_with("min_")) %>%
          dplyr::rename_with( ~ stringr::str_remove_all(., "max_"), dplyr::contains("max_")) %>%
          CDMConnector::computeQuery()
      }

    } else {
      result_dt_w <- result_dt
    }

    result_cb <- result_cb %>% dplyr::left_join(result_cb_w, by = c("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id")) %>%
      CDMConnector::computeQuery()
    result_dt <- result_dt %>% dplyr::left_join(result_dt_w, by = c("subject_id", "cohort_start_date", "cohort_end_date", "cohort_definition_id")) %>%
      CDMConnector::computeQuery()

  }

  #drop columns not needed
  valueDrop <- c("number", "binary", "date", "time") %>%
    dplyr::setdiff(value)


  # join result_cb and result_dt together, tidy up and select
  result_all <-
    result_cb %>% dplyr::inner_join(
      result_dt,
      by = c(
        "subject_id",
        "cohort_definition_id",
        "cohort_start_date",
        "cohort_end_date"
      )
    ) %>% dplyr::select("cohort_definition_id", dplyr::everything()) %>%
    dplyr::select(-dplyr::starts_with(valueDrop)) %>%
    CDMConnector::computeQuery()

  result_all <- result_all %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "binary_"), dplyr::contains("binary_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "number_"), dplyr::contains("number_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "date_"), dplyr::contains("date_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "time_"), dplyr::contains("time_")) %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()

  # Rename new columns in results if previously existing in x
  colnames_repeated <- colnames(result_all)[colnames(result_all) %in% colnames(x)]
  colnames_repeated <- colnames_repeated[!(colnames_repeated %in% c("cohort_definition_id", "cohort_start_date", "cohort_end_date", "subject_id"))]

  for(col in colnames_repeated) {
    num <- 1
    col_new <- paste0(col,"_", num)
    while(col_new %in% colnames(x)) {
      num <- num + 1
      col_new <- paste0(col,"_",num)
    }
    col_new <- rlang::enquo(col_new)
      result_all <- result_all %>%
      dplyr::rename(!!col_new := col) %>%
        CDMConnector::computeQuery()
  }

  if("overlap_id" %in% colnames(x)) {
    result_all <- result_all %>%
      dplyr::left_join(x, by = c("cohort_definition_id", "cohort_start_date", "cohort_end_date", "subject_id")) %>%
      CDMConnector::computeQuery()
  } else {
    result_all <- result_all %>%
      dplyr::left_join(x, by = c("cohort_definition_id", "cohort_start_date", "cohort_end_date", "subject_id")) %>%
      CDMConnector::computeQuery()
  }


  if(is.null(tablePrefix)){
    result_all <- result_all %>%
      CDMConnector::computeQuery()
  } else {
    result_all <- result_all %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }

  return(result_all)
}
