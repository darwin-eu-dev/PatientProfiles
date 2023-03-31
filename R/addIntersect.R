
#' It creates columns to indicate overlaps information
#'
#' @param x table containing the individual for which the overlap indicator to
#' be attached as extra columns
#' @param cdm cdm containing the tables
#' @param tableName name of the cohort that we want to check for overlap
#' @param cohortId vector of cohort definition ids to include
#' @param value value of interest to add: it can be count, flag, date or time
#' @param window window to consider events of
#' @param indexDate date of reference in table x
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
#' @export
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
#'tableName = "cohort2", value = "date") %>% dplyr::collect()
#'}
#'
addIntersect <- function(x,
                         cdm,
                         tableName,
                         cohortId = NULL,
                         value, # must be only one of the four now
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

  tableCheck <- tableName %in% names(cdm)
  if (!isTRUE(tableCheck)) {
    errorMessage$push("- `tableName` is not found in cdm")
  }
  checkmate::assert_integerish(cohortId, null.ok = TRUE)

  window <- checkWindow(window)

  checkmate::assert_character(value, len = 1)
  valueCheck <-
    value %>% dplyr::intersect(c("count", "flag", "date", "time")) %>% dplyr::setequal(value)
  if (!isTRUE(valueCheck)) {
    errorMessage$push("- `value` must be either 'count','flag','date' or 'time' ")
  }

  orderCheck <- order %in% c("first", "last")
  if (!isTRUE(orderCheck)) {
    errorMessage$push("- `order` must be either 'first' or 'last' ")
  }

  checkmate::assert_character(indexDate, len = 1)
  checkmate::assert_character(targetStartDate, len = 1)
  checkmate::assert_character(targetEndDate, len = 1, null.ok = TRUE)

  columnCheck <- targetStartDate %in% colnames(cdm[[tableName]])
  if (!isTRUE(columnCheck)) {
    errorMessage$push("- `targetStartdate` is not a column of cdm[[tableName]]")
  }

  column2Check <- indexDate %in% colnames(x)
  if (!isTRUE(column2Check)) {
    errorMessage$push("- `indexDate` is not a column of x")
  }

  if(!is.null(targetEndDate)) {
    column3Check <- targetEndDate %in% colnames(cdm[[tableName]])
    if (!isTRUE(column3Check)) {
      errorMessage$push("- `targetEndDate` is not a column of cdm[[tableName]]")
    }
  }

  if(!is.null(cohortId)) {
  # Get name of cohorts if available in cdm
    tryCatch(
      {cohortName <- CDMConnector::cohortSet(cdm[[tableName]])
      },
      error = function(e) {}
    )
  # If we cannot get the cohort names, let them be named after the cohort ids
  if(is.null(cohortName)) {
    cohortName <- tibble::tibble(cohort_definition_id = cohortId, cohort_name = paste0("cohort",cohortId))
  }
  }
  # If there are no cohorts ids, let the cohort names be "all", implemented directly in the loop

  # Check nameStyle provided contains "window" and "cohortName" parameters
  parameters <- list("cohortName" = cohortName, "window" = window)
  checkName(nameStyle, parameters)

  # Change nameStyle to current/new parameter names
  nameStyle <- sub("window", "window_name", nameStyle)
  nameStyle <- sub("cohortName", "cohort_name", nameStyle)

  # Get window names as characters for column names
  windowNames <- getWindowNames(window)

    checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  if("person_id" %in% colnames(x) && ("subject_id" %in% colnames(x))) {
    errorMessage$push("- both `subject_id` and `person_id` cannot be columns of x")
  }

  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  # Get concept id name of the intersection table
  get_concept <- list(
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id"
  )
    conceptIdname <- get_concept[[tableName]]

    if(is.null(conceptIdname)) {
      conceptIdname <- "cohort_definition_id"
    }

  # define overlapcohort table from cdm containing the events of interest
  overlapCohort <- cdm[[tableName]]

  # generate overlappingcohort using code from getoverlappingcohort
  # filter by cohortId
  if (!is.null(cohortId) && !is.null(targetEndDate)) {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = .env$targetStartDate,
        "overlap_end_date" = .env$targetEndDate,
        "overlap_id" = dplyr::all_of(conceptIdname)
      ) %>% dplyr::filter(.data$overlap_id %in% .env$cohortId) %>%
      CDMConnector::computeQuery()
  } else if(!is.null(cohortId) && is.null(targetEndDate)) {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = .env$targetStartDate,
        "overlap_id" = dplyr::all_of(conceptIdname)
      ) %>% dplyr::filter(.data$overlap_id %in% .env$cohortId) %>%
      dplyr::mutate(overlap_end_date = .data$overlap_start_date) %>%
      dplyr::select("overlap_start_date", "overlap_id", "overlap_end_date", "subject_id") %>%
      CDMConnector::computeQuery()
  } else if (is.null(cohortId) && !is.null(targetEndDate)){
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = .env$targetStartDate,
        "overlap_end_date" = .env$targetEndDate,
        "overlap_id" = dplyr::all_of(conceptIdname)
      ) %>%
      CDMConnector::computeQuery()
  } else {
    overlapCohort <- overlapCohort %>%
      dplyr::rename(
        "overlap_start_date" = .env$targetStartDate,
        "overlap_id" = dplyr::all_of(conceptIdname)
      ) %>%
      dplyr::mutate(overlap_end_date = .data$overlap_start_date) %>%
      dplyr::select("overlap_start_date", "overlap_id", "overlap_end_date", "subject_id") %>%
      CDMConnector::computeQuery()
  }

  if("person_id" %in% colnames(x) && !("subject_id" %in% colnames(x))) {
    x <- x %>%
      dplyr::rename("subject_id" = "person_id") %>%
      CDMConnector::computeQuery()
  }

    result_cb <- x %>%
      dplyr::select("subject_id", dplyr::all_of(indexDate)) %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery()
    result_dt <- x %>%
      dplyr::select("subject_id", dplyr::all_of(indexDate)) %>%
      dplyr::distinct() %>%
      CDMConnector::computeQuery()
    result <- x %>%
      dplyr::select("subject_id", dplyr::all_of(indexDate)) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(overlapCohort, by = "subject_id") %>%
      CDMConnector::computeQuery()
    columns_interest <- c("subject_id", "overlap_id", indexDate)

  # what if tablePrefix is not NULL? same for all compute -> isn't this okay? we do temporary until in the end we put in permanent table #K

  # Start loop for different windows
  for (i in c(1:length(window))) {
    workingWindow <- window[[i]]
    window_name <- windowNames[[i]]

    result_w <- result

    indexDate <- as.character(indexDate)

    if (workingWindow[2] != Inf) {
      result_w <- result_w %>%
      dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
      CDMConnector::dateadd(date = "overlap_start_date",
                            number = !!-workingWindow[2])
                            ))) %>%
      dplyr::filter(.data[[indexDate]] >= .data$overlap_start_date) %>%
      CDMConnector::computeQuery()
      }
    if (workingWindow[1] != -Inf) {
      result_w <- result_w %>%
      dplyr::mutate(overlap_end_date = as.Date(dbplyr::sql(
            CDMConnector::dateadd(date = "overlap_end_date",
                                  number = !!-workingWindow[1])
          ))) %>%
      dplyr::filter(.data[[indexDate]] <= .data$overlap_end_date) %>%
      CDMConnector::computeQuery()
      }

    # add count or flag
    if ("count" %in% value | "flag" %in% value) {
      result_cb_w <- result_w %>%
        dplyr::select(dplyr::all_of(columns_interest)) %>%
        dplyr::group_by(.data$subject_id, .data[[indexDate]], .data$overlap_id) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(
          flag = 1,
          overlap_id = as.numeric(.data$overlap_id),
          window_name = window_name
        ) %>%
        CDMConnector::computeQuery()
        if(!is.null(cohortId)) {
        result_cb_w <- result_cb_w %>%
        dplyr::left_join(cohortName, by = c("overlap_id" = "cohort_definition_id"), copy = TRUE) %>%
        CDMConnector::computeQuery()
        } else {
        result_cb_w <- result_cb_w %>%
        dplyr::mutate(cohort_name = "all") %>%
        CDMConnector::computeQuery()
        }
        result_cb_w <- result_cb_w %>%
        dplyr::select(-"overlap_id") %>%
        tidyr::pivot_wider(
          names_from = c("cohort_name", "window_name"),
          values_from = c("count", "flag"),
          names_glue = paste0("{.value}_",nameStyle),
          values_fill = 0
        )  %>%
        dplyr::right_join(x %>% dplyr::select("subject_id", dplyr::all_of(indexDate)),
                          by = c("subject_id", dplyr::all_of(indexDate))) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with(c(
          "flag", "count"
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
        dplyr::select("overlap_start_date", dplyr::all_of(columns_interest)) %>%
        dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!+workingWindow[2])
        ))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$subject_id, .data$overlap_id, .data[[indexDate]]) %>%
        dplyr::mutate(
          min_date = min(.data$overlap_start_date),
          max_date = max(.data$overlap_start_date)
        ) %>%
        dplyr::mutate(
          min_time = !!CDMConnector::datediff(indexDate, "min_date", interval = "day"),
          max_time = !!CDMConnector::datediff(indexDate, "max_date", interval = "day")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"overlap_start_date") %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          overlap_id = as.numeric(.data$overlap_id),
          window_name = window_name
        ) %>%
        CDMConnector::computeQuery()
        if(!is.null(cohortId)) {
        result_dt_w <- result_dt_w %>%
        dplyr::left_join(cohortName, by = c("overlap_id" = "cohort_definition_id"), copy = TRUE) %>%
        CDMConnector::computeQuery()
        } else {
        result_dt_w <- result_dt_w %>%
        dplyr::mutate(cohort_name = "all") %>%
        CDMConnector::computeQuery()
        }
        # Get min or max date/time depending on order parameter
      if (order == "first") {
        result_dt_w <-
          result_dt_w  %>%
          dplyr::group_by(subject_id, cohort_name, .data[[indexDate]]) %>%
          dplyr::select(-dplyr::starts_with("max_")) %>%
          dplyr::filter(min_date == min(min_date)) %>%
          dplyr::select(-"overlap_id") %>%
          tidyr::pivot_wider(
            names_from = c("cohort_name", "window_name"),
            values_from = c("min_time", "min_date"),
            names_glue = paste0("{.value}_",nameStyle),
            values_fill = NA
          ) %>%
          dplyr::rename_with( ~ stringr::str_remove_all(., "min_"), dplyr::contains("min_")) %>%
          CDMConnector::computeQuery()
      } else {
        result_dt_w <-
          result_dt_w  %>%
          dplyr::group_by(subject_id, cohort_name, .data[[indexDate]]) %>%
          dplyr::select(-dplyr::starts_with("min_")) %>%
          dplyr::filter(max_date == max(max_date)) %>%
          dplyr::select(-"overlap_id") %>%
          tidyr::pivot_wider(
            names_from = c("cohort_name", "window_name"),
            values_from = c("max_time", "max_date"),
            names_glue = paste0("{.value}_",nameStyle),
            values_fill = NA
          ) %>%
          dplyr::rename_with( ~ stringr::str_remove_all(., "max_"), dplyr::contains("max_")) %>%
          CDMConnector::computeQuery()
      }

    } else {
      result_dt_w <- result_dt
    }

      result_cb <- result_cb %>% dplyr::left_join(result_cb_w, by = c("subject_id", dplyr::all_of(indexDate))) %>%
        CDMConnector::computeQuery()
      result_dt <- result_dt %>% dplyr::left_join(result_dt_w, by = c("subject_id", dplyr::all_of(indexDate))) %>%
        CDMConnector::computeQuery()
  }

  #drop columns not needed
  valueDrop <- c("count", "flag", "date", "time") %>%
    dplyr::setdiff(value)

  # join result_cb and result_dt together, tidy up and select
    result_all <-
      result_cb %>% dplyr::inner_join(
        result_dt,
        by = c("subject_id", dplyr::all_of(indexDate))) %>%
      dplyr::select(-dplyr::starts_with(valueDrop)) %>%
      CDMConnector::computeQuery()


  result_all <- result_all %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "flag_"), dplyr::contains("flag_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "count_"), dplyr::contains("count_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "date_"), dplyr::contains("date_")) %>%
    dplyr::rename_with( ~ stringr::str_remove_all(., "time_"), dplyr::contains("time_")) %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()

  # Rename new columns in results if previously existing in x
  colnames_repeated <- colnames(result_all)[colnames(result_all) %in% colnames(x)]
  colnames_repeated <- colnames_repeated[!(colnames_repeated %in% c(conceptIdname, "cohort_start_date", "cohort_end_date", "subject_id"))]

  for(col in colnames_repeated) {
    col_x <- col
    num <- 1
    col_new <- paste0(col,"_", num)
    while(col_new %in% colnames(x)) {
      num <- num + 1
      col_x <- col_new
      col_new <- paste0(col,"_",num)
    }
    warning("New column has been named `",col_new,"` because `", col_x,"` already exists in x")

    col_new <- rlang::enquo(col_new)
      result_all <- result_all %>%
      dplyr::rename(!!col_new := col) %>%
        CDMConnector::computeQuery()
  }

  if("overlap_id" %in% colnames(result_all)) {
    result_all <- result_all %>%
      dplyr::select(-"overlap_id") %>%
      CDMConnector::computeQuery()
  }

  if("cohort_name" %in% colnames(result_all)) {
    result_all <- result_all %>%
      dplyr::select(-"cohort_name") %>%
      CDMConnector::computeQuery()
  }

  # Get all columns back from x
    result_all <- result_all %>%
      dplyr::left_join(x, by = c("subject_id", dplyr::all_of(indexDate))) %>%
      CDMConnector::computeQuery()

  # Check this now works for instance if cohort without cohort_def_id and not given cohortIds. Or for death and add come condition occurrence #K

  # Compute permanently if asked
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
