#' Create a gt table from a summary results objects
#'
#' @param table Long table.
#' @param filterRow Filter variables.
#' @param pivotWider List of columns to compare.
#' @param hideColumn Columns to hide.
#'
#' @return New table in gt format
#'
#' @export
tableSummary <- function(table,
                         filterRow = NULL,
                         pivotWider = NULL,
                         hideColumn = NULL) {
  # initial checks

  # filter data
  for (k in seq_along(filterRow)) {
    table <- table %>%
      dplyr::filter(.data[[names(filterRow)[k]]] %in% filterRow[[k]])
  }

  # pivot wider columns
  if (length(pivotWider) > 0) {
    if (is.null(names(pivotWider))) {
      namesPivotWider <- pivotWider
    } else {
      namesPivotWider <- names(pivotWider)
      names(pivotWider) <- NULL
    }
    namesColumns <- table %>%
      dplyr::select(dplyr::all_of(pivotWider)) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(pivotWider))) %>%
      dplyr::mutate(names_columns = paste0(
        "estimate_", dplyr::row_number()
      ))
    newNamesColumns <- namesColumns %>%
      tidyr::pivot_longer(
        dplyr::all_of(pivotWider), names_to = "old_name", values_to = "value"
      ) %>%
      tidyr::pivot_wider(
        names_from = "names_columns", values_from = "value"
      ) %>%
      dplyr::inner_join(
        dplyr::tibble(old_name = pivotWider, variable = namesPivotWider),
        by = "old_name"
      ) %>%
      dplyr::select(-"old_name") %>%
      dplyr::mutate(estimate_type = as.character(NA)) %>%
      dplyr::relocate("estimate_type")
    table <- table %>%
      dplyr::inner_join(namesColumns, by = dplyr::all_of(pivotWider)) %>%
      dplyr::select(-dplyr::all_of(pivotWider)) %>%
      tidyr::pivot_wider(names_from = "names_columns", values_from = "estimate")
    table <- newNamesColumns %>%
      dplyr::bind_rows(table) %>%
      dplyr::relocate(
        dplyr::starts_with("estimate"), .after = dplyr::last_col()
      )
  }

  # hide columns
  if (length(hideColumn) > 0) {
    table <- table %>%
      dplyr::select(-dplyr::all_of(hideColumn))
  }

  return(table)
}

#' Create a gt table from a summary characteristics object
#'
#' @param table Summary characteristics long table.
#'
#' @return New table in gt format
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     "Visits" = list(
#'       tableName = "visit_occurrence", value = "count", window = c(-365, 0)
#'      )
#'   ),
#'   cohortIntersect = list(
#'     "Medications" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
#'     )
#'   ),
#'   minCellCount = 1
#' ) %>%
#'   tableCharacteristics()
#'}
tableCharacteristics <- function(table) {
  table %>%
    dplyr::mutate(
      group_level = paste0(.data$group_name, ": ", .data$group_level),
      strata_level = paste0(.data$strata_name, ": ", .data$strata_level)
    ) %>%
    tableSummary(
      pivotWider = c(
        "CDM name" = "cdm_name", "Group" = "group_level",
        "Strata" = "strata_level"
      ),
      hideColumn = c(
        "group_name", "strata_name", "result_type", "variable_type"
      )
    )
}
