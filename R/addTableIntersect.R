#' It creates columns to indicate overlap information with table in the cdm
#'
#' @param x table containing the individuals for which the overlap indicator
#' to be attached as extra columns
#' @param cdm cdm containing the tables
#' @param tableName name of the table that we want to check for overlap
#' @param value value of interest to add: it can be number, binary, or a column
#' of cdm[[tableName]]
#' @param window window to consider events of, from date of reference in table x
#' to date of event at event table
#' @param filter filter for the cdm[[tableName]] if provided
#' @param order last or first value to use, if not number or binary
#' @param name naming of the added column
#' @param compute compute/not compute
#'
#' @return table x with added overlap information
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort2 %>% addTableIntersect(cdm, tableName = "drug_exposure",
#' value = "number", window = c(NA,-1), filter= drug_concept_id == 2)
#' # This will add a column to the individuals in cohort2 with the number of
#' drug exposures of drug with drug_concept_id 2, previous to the index event
#' of the occurrence
#' }
#'
addTableIntersect <- function(x,
                               cdm,
                               tableName,
                               value = c("number", "binary"),
                               window = c(0, NA),
                               filter = NULL,
                               order = "first",
                               name = "{value}_{tableName}_{window}",
                               compute = TRUE) {

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
  checkmate::assert_integerish(window, len = 2, null.ok = TRUE)

  value_possibilities <- c("number","binary", colnames(cdm[[tableName]]))

  valueCheck <- all(value %in% value_possibilities)
  if (!isTRUE(valueCheck)) {
    errorMessage$push("- `value` must be either 'number','binary' or a column of cdm[[tableName]] ")
  }

  orderCheck <- order %in% c("first", "last")
  if (!isTRUE(orderCheck)) {
    errorMessage$push("- `order` must be either 'first' or 'last' ")
  }

  checkmate::assert_character(name, len = 1)

  checkmate::reportAssertions(collection = errorMessage)


  # Start code
  # define overlap table from cdm containing the events of interest
  overlaptable <- cdm[[tableName]]

  # apply filter to the overlap table
  tryCatch({
        overlaptable <- overlaptable %>%
          dplyr::filter({{filter}})
  }, error = function(e){
    message("An error occurred while filtering cdm[[tableName]]")
    message(e)
  }, warning = function(w) {
    message("A warning occurred while filtering cdm[[tableName]]")
    message(w)
  }
  )

  # get start and end date column names from the desired table
  name_start_date <- overlaptable %>% dplyr::select(dplyr::ends_with("start_date")) %>% colnames()
  name_end_date <- overlaptable %>% dplyr::select(dplyr::ends_with("end_date")) %>% colnames()

  overlaptable <- overlaptable %>% dplyr::mutate(
    "overlap_start_date" = .data[[name_start_date]],
    "overlap_end_date" = .data[[name_end_date]],
    "subject_id" = person_id
    ) %>% dplyr::select(-person_id)

  result <- x %>%
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
    dplyr::distinct() %>%
    dplyr::inner_join(overlaptable, by = "subject_id")

  if (!is.na(window[2])) {
    result <- result %>%
      dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
        CDMConnector::dateadd(date = "overlap_start_date",
                              number = !!-window[2])
      ))) %>%
      dplyr::filter(.data$cohort_start_date >= .data$overlap_start_date)
  }
  if (!is.na(window[1])) {
    result <- result %>%
      dplyr::mutate(overlap_end_date = as.Date(dbplyr::sql(
        CDMConnector::dateadd(date = "overlap_end_date",
                              number = !!-window[1])
      ))) %>%
      dplyr::filter(.data$cohort_start_date <= .data$overlap_end_date)
  }

  # get the window as character for the naming of the output columns later
  window_pre <- ifelse(is.na(window[1]),"NA",as.character(window[1]))
  window_post <- ifelse(is.na(window[2]),"NA",as.character(window[2]))

  # add count and binary
  if ("number" %in% value | "binary" %in% value) {
    result_cb <- result %>%
      dplyr::select("subject_id",
                    "cohort_start_date",
                    "cohort_end_date"
                    ) %>%
      dplyr::group_by(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) %>%
      dplyr::summarise(number = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(
        binary = 1,
        overlapTableName = .env$tableName
      ) %>%
      dplyr::mutate(
        window_char = paste0("(",.env$window_pre,",",.env$window_post,")")
      ) %>%
      tidyr::pivot_wider(
        names_from = c("overlapTableName","window_char"),
        values_from = c("number", "binary"),
        names_glue = "{.value}_{overlapTableName}_{window_char}",
        values_fill = 0
      )  %>%
      dplyr::right_join(x,
                        by = c("subject_id", "cohort_start_date", "cohort_end_date")) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with(c(
        "binary", "number"
      )),
      ~ dplyr::if_else(is.na(.x), 0, .x)))

  } else {
    result_cb <- x
  }

  # add other values from columns in cdm[[tableName]]
  if (any(colnames(cdm[[tableName]]) %in% value)) {
    window <- ifelse(is.na(window),0,window)
    user_columns <-  colnames(cdm[[tableName]])[colnames(cdm[[tableName]]) %in% value]
    counter <- 0
    for(working_column in user_columns) {
      # first result
      result_dt_min <- result %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date",
          "overlap_start_date",
          dplyr::all_of(working_column)
        ) %>% dplyr::distinct() %>% dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!+window[2])
        ))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) %>%
        dplyr::filter(.data$overlap_start_date == min(.data$overlap_start_date)) %>%
        dplyr::mutate(min_value = .data[[working_column]]) %>%
        dplyr::select(-dplyr::all_of(working_column)) %>%
        dplyr::ungroup() %>% dplyr::distinct() %>% dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!-window[2])
        )))

      # last result
      result_dt_max <- result %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date",
          "overlap_start_date",
          dplyr::all_of(working_column)
        ) %>% dplyr::distinct() %>% dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!+window[2])
        ))) %>%
        dplyr::distinct() %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) %>%
        dplyr::filter(.data$overlap_start_date == max(.data$overlap_start_date)) %>%
        dplyr::mutate(max_value = .data[[working_column]]) %>%
        dplyr::select(-dplyr::all_of(working_column)) %>%
        dplyr::ungroup() %>% dplyr::distinct() %>% dplyr::mutate(overlap_start_date = as.Date(dbplyr::sql(
          CDMConnector::dateadd(date = "overlap_start_date",
                                number = !!-window[2])
        )))

      # combine both
      result_dt <- result %>%
        dplyr::select(
          "subject_id",
          "cohort_start_date",
          "cohort_end_date",
          "overlap_start_date",
          dplyr::all_of(working_column)
        ) %>% dplyr::distinct() %>%
        dplyr::left_join(result_dt_min, by = c("subject_id","cohort_start_date","cohort_end_date","overlap_start_date")) %>%
        dplyr::left_join(result_dt_max, by = c("subject_id","cohort_start_date","cohort_end_date","overlap_start_date")) %>%
        dplyr::group_by(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) %>%
        dbplyr::window_order(overlap_start_date) %>%
        tidyr::fill(min_value, .direction = "down") %>%
        tidyr::fill(max_value, .direction = "up") %>%
        dbplyr::window_order() %>%
        dplyr::select(-c("overlap_start_date")) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          overlapTableName = .env$tableName
        ) %>%
        dplyr::mutate(
          columnname = .env$working_column
        ) %>%
        dplyr::mutate(
          window_char = paste0("(",.env$window_pre,",",.env$window_post,")")
        ) %>%
        tidyr::pivot_wider(
          names_from = c("overlapTableName","columnname","window_char"),
          values_from = c("min_value", "max_value"),
          names_glue = "{.value}_{columnname}_{overlapTableName}_{window_char}",
          values_fill = NA
        )  %>%
        dplyr::select(-dplyr::all_of(working_column)) %>%
        dplyr::distinct() %>%
        dplyr::right_join(x,
                          by = c("subject_id", "cohort_start_date", "cohort_end_date"))

      if(counter == 0) {
        result_dt_final <- result_dt
      } else {
        result_dt_final <- result_dt_final %>% dplyr::full_join(result_dt,
                                                                by = c("subject_id", "cohort_start_date", "cohort_end_date","cohort_definition_id"))
      }

      counter <- counter + 1
    }

    result_dt <- result_dt_final

    if (order == "first") {
      result_dt <-
        result_dt  %>%
        dplyr::select(-dplyr::starts_with("max_")) %>%
        dplyr::rename_with( ~ stringr::str_remove_all(., "min_value_"), dplyr::contains("min_"))
    } else
    {
      result_dt <-
        result_dt  %>%
        dplyr::select(-dplyr::starts_with("min_")) %>%
        dplyr::rename_with( ~ stringr::str_remove_all(., "max_value_"), dplyr::contains("max_"))
    }

  } else {
    result_dt <- x
  }

  # drop columns not needed
  valueDrop <- c("number", "binary", colnames(cdm[[tableName]])[colnames(cdm[[tableName]]) %in% value]) %>%
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
    dplyr::select(-dplyr::starts_with(valueDrop))

  if (isTRUE(compute)) {
    result_all <- result_all %>% dplyr::compute()
  }

  return(result_all)
}
