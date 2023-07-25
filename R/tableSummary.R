#' Create a gt table from a summary results objects
#'
#' @param table Long table.
#' @param filterRow Filter variables.
#' @param pivotWider List of columns to compare.
#' @param hideColumn Columns to hide.
#'
#' @return New table in gt format
#'
#' @noRd
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
#' @noRd
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

#' Give format to a summary object.
#'
#' @param summaryResult A SummarisedResult object.
#' @param format A list of formats of teh estimate_type.
#' @param decimals Number of decimals to round each estimate_type.
#' @param keepNotFromatted Whether to keep non formatted lines.
#'
#' @return A formatted summarisedResult.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   summariseCharacteristics(
#'     ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'     minCellCount = 1
#'   ) %>%
#'   formatEstimates(
#'     format = c(
#'       "N (%)" = "count (percentage%)",
#'       "N" = "count",
#'       "median [Q25-Q75]" = "median [q25-q75]"
#'     ),
#'     decimals = c(count = 0),
#'     keepNotFromatted = FALSE
#'   )
#' }
#'
formatEstimates <- function(summaryResult,
                            format = c(),
                            keepNotFromatted = TRUE,
                            decimals = c(count = 0),
                            defaultDecimal = 2,
                            decimalMark = ".",
                            bigMark = ",") {
  # initial checks
  #checkInput(
  #  summaryResult = summaryResult, format = format, decimals = decimals,
  #  keepNotFormatted = keepNotFormatted
  #)

  # format decimals
  summaryResult <- formatNumbers(
    summaryResult, decimals, defaultDecimal, decimalMark, bigMark
  )

  # tidy estimates
  summaryResult <- tidyEstimates(summaryResult, format, keepNotFromatted)

  return(summaryResult)
}

tidyEstimates <- function(summaryResult, format, keepNotFromatted) {
  # tidy by groups
  label <- names(format)
  if (is.null(label)) {
    label <- format
  } else {
    label <- ifelse(label == "", format, label)
  }
  toGroup <- names(summaryResult)
  toGroup <- toGroup[!(toGroup %in% c("estimate", "estimate_type"))]
  summaryResult <- summaryResult %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(toGroup))) %>%
    dplyr::mutate(formats = paste0(.data$estimate_type, collapse = " ")) %>%
    dplyr::ungroup()
  formattedResult <- NULL
  allEstimates <- unique(summaryResult$estimate_type)
  for (k in seq_along(format)) {
    estimates <- allEstimates[sapply(allEstimates, grepl, format[k])]
    toEvaluate <- getEvaluate(format[k], estimates)
    toFotmat <- summaryResult %>%
      dplyr::rowwise() %>%
      dplyr::filter(sum(.data$estimate_type == .env$estimates) == 1) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(toGroup))) %>%
      dplyr::filter(dplyr::n() == length(.env$estimates)) %>%
      dplyr::ungroup()
    summaryResult <- summaryResult %>%
      dplyr::anti_join(toFotmat, by = names(summaryResult))
    formattedResult <- formattedResult %>%
      dplyr::union_all(
        toFotmat %>%
          tidyr::pivot_wider(
            names_from = "estimate_type", values_from = "estimate"
          ) %>%
          dplyr::mutate(
            format = .env$label[k],
            estimate = !!rlang::parse_expr(toEvaluate)
          ) %>%
          dplyr::select(-dplyr::all_of(estimates))
      )
  }

  # keep formatted?
  if (keepNotFromatted == TRUE) {
    formattedResult <- formattedResult %>%
      dplyr::union_all(
        summaryResult %>% dplyr::rename("format" = "estimate_type")
      )
  }
  formattedResult <- formattedResult %>%
    dplyr::select(-"formats")

  return(formattedResult)
}

getEvaluate <- function(format, estimates) {
  toEvaluate <- format
  for (j in seq_along(estimates)) {
    toEvaluate <- gsub(
      estimates[j], paste0("', .data$", estimates[j], ", '"), toEvaluate
    )
  }
  toEvaluate <- paste0("paste0('", toEvaluate, "')")
  return(toEvaluate)
}

formatNumbers <- function(summaryResult,
                          decimals,
                          defaultDecimal,
                          decimalMark,
                          bigMark) {
  summaryResult <- summaryResult %>%
    dplyr::mutate(
      is_numeric = !is.na(suppressWarnings(as.numeric(.data$estimate))),
      formatted = FALSE
    )
  for (k in seq_along(decimals)) {
    summaryResult <- summaryResult %>%
      dplyr::mutate(estimate = formatEst(
        .data$estimate_type == names(decimals)[k] & .data$is_numeric &
          .data$formatted == FALSE,
        .data$estimate, decimals[k], bigMark, decimalMark
      )) %>%
      dplyr::mutate(formatted = dplyr::if_else(
        .data$estimate_type == names(decimals)[k] & .data$is_numeric &
          .data$formatted == FALSE,
        TRUE,
        .data$formatted
      ))
  }
  summaryResult <- summaryResult %>%
    dplyr::mutate(estimate = formatEst(
      .data$is_numeric & .data$formatted == FALSE, .data$estimate, decimals[k],
      bigMark, decimalMark
    )) %>%
    dplyr::select(-"is_numeric", -"formatted")

  return(summaryResult)
}

formatEst <- function(condition, x, dec, bm, dm) {
  xnew <- x
  xnew[condition] <- base::format(
    round(as.numeric(x[condition]), dec), nsmall = dec, big.mak = bm,
    decimal.mark = dm
  )
  return(xnew)
}
