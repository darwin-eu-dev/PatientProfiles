#' Create a gt table from a summary results objects
#'
#' @param table Long table.
#' @param filter Filter variables.
#' @param pivotWider List of columns to compare.
#' @param hide Columns to hide.
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
#' cdm$cohort1 %>%
#'   summariseCharacteristics(cdm = cdm, minCellCount = 1) %>%
#'   tableSummary()
#' }
#'
tableSummary <- function(table,
                         filterRow = NULL,
                         pivotWider = NULL,
                         estimateColumn = "estimate",
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
      dplyr::mutate(names_columns = paste0("estimate_", dplyr::row_number()))
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
      dplyr::select(-"old_name")
    table <- table %>%
      dplyr::inner_join(namesColumns, by = dplyr::all_of(pivotWider)) %>%
      dplyr::select(-dplyr::all_of(pivotWider)) %>%
      tidyr::pivot_wider(
        names_from = "names_columns",
        values_from = dplyr::all_of(estimateColumn)
      )
    table <- newNamesColumns %>%
      dplyr::bind_rows(table)
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
#' cdm$cohort1 %>%
#'   summariseCharacteristics(
#'     cdm = cdm, minCellCount = 1, covariates = list(cohort2 = c(-Inf, -1))
#'   ) %>%
#'   tableCharacteristics()
#' }
#'
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
        "group_name", "strata_name", "variable_type", "generated_by"
      )
    )
}
