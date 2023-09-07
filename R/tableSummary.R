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
#' @param settings Columns that contain the setting, the name point to the
#' label.
#' @param variable Name of the clumn that contains the variable name and level.
#' @param format A list of formats of teh estimate_type.
#' @param keepNotFromatted Whether to keep non formatted lines.
#' @param decimals Number of decimals to round each estimate_type.
#' @param defaultDecimal Number of decimals by default.
#' @param decimalMark Decimal mark.
#' @param bigMark Big mark.
#'
#' @return A formatted summarisedResult.
#'
#' @noRd
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
#'   formatSummarisedResults(
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
formatSummarisedResults <- function(summaryResult,
                            long = list(
                              "Variable" = c(level = "variable"),
                              "Level" = c(level = "variable_level"),
                              "Format" = c(level = "format")
                            ),
                            wide = list(
                              "CDM Name" = c(level = "cdm_name"),
                              "Group" = c(level = c("group_name", "group_level")),
                              "Strata" = c(level = c("strata_name", "strata_level"))
                            ),
                            format = c(
                              "N (%)" = "count (percentage%)",
                              "median [min; q25 - q75; mx]",
                              "mean (sd)",
                              "median [q25 - q75]",
                              "N" = "count"
                            ),
                            keepNotFromatted = TRUE,
                            decimals = c(default = 0),
                            decimalMark = ".",
                            bigMark = ",",
                            style = c(defaultWidth = "200", separator = c("Format"))) {
  # initial checks
  #checkInput(
  #  summaryResult = summaryResult, long = long, wide = wide, format = format,
  #  estimateColumn = estimateColumn, keepNotFormatted = keepNotFormatted,
  #  decimals = decimals, decimalMark = decimalMark, bigMark = bigMark
  #)

  # format decimals
  summaryResult <- formatNumbers(summaryResult, decimals, decimalMark, bigMark)

  # tidy estimates
  summaryResult <- tidyEstimates(summaryResult, format, keepNotFromatted)

  # select only needed columns
  summaryResult <- selectVariables(summaryResult, wide, long)

  # pivot wide
  summaryResult <- pivotWide(summaryResult, wide)

  # arrange long
  summaryResult <- arrangeLong(summaryResult, long)

  # create gt object
  summaryTable <- summaryResult %>%
    gt::gt() %>%
    styleTable(long, wide, style, attr(summaryResult, "column_labels"))

  return(summaryTable)
}

formatNumbers <- function(summaryResult, decimals, decimalMark, bigMark) {
  summaryResult <- summaryResult %>%
    dplyr::mutate(
      is_numeric = !is.na(suppressWarnings(as.numeric(.data$estimate))),
      formatted = FALSE
    )
  defaultDecimal <- decimals["default"]
  decimals <- decimals[names(decimals) != "default"]
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
      .data$is_numeric & .data$formatted == FALSE, .data$estimate,
      defaultDecimal, bigMark, decimalMark
    )) %>%
    dplyr::select(-"is_numeric", -"formatted")

  return(summaryResult)
}
formatEst <- function(condition, x, dec, bm, dm) {
  xnew <- x
  xnew[condition] <- gsub(" ", "", base::format(
    round(as.numeric(x[condition]), dec), nsmall = dec, big.mark = bm,
    decimal.mark = dm
  ))
  return(xnew)
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
  allEstimates <- formats$format_key %>% unique()
  for (k in seq_along(format)) {
    estimates <- allEstimates[sapply(allEstimates, grepl, format[k])]
    toEvaluate <- getEvaluate(format[k], estimates)
    toFormat <- summaryResult %>%
      dplyr::rowwise() %>%
      dplyr::filter(sum(.data$estimate_type == .env$estimates) == 1) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(toGroup))) %>%
      dplyr::filter(dplyr::n() == length(.env$estimates)) %>%
      dplyr::ungroup()
    if (nrow(toFormat) > 0) {
      summaryResult <- summaryResult %>%
        dplyr::anti_join(toFormat, by = names(summaryResult))
      formattedResult <- formattedResult %>%
        dplyr::union_all(
          toFormat %>%
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
selectVariables <- function(summaryResult, wide, long) {
  extractNames <- function(x) {
    x[substr(names(x), 1, 5) == "level" | substr(names(x), 1, 4) == "name"] %>%
      unname()
  }
  wideColumns <- lapply(wide, extractNames) %>% unlist() %>% unname()
  longColumns <- lapply(long, extractNames) %>% unlist() %>% unname()
  summaryResult <- summaryResult %>%
    dplyr::select(dplyr::all_of(c(longColumns, wideColumns, "estimate")))
  return(summaryResult)
}
pivotWide <- function(summaryResult, wide) {
  for (k in seq_along(wide)) {
    x <- wide[[k]]
    level <- x[substr(names(x), 1, 5) == "level"] %>% unname()
    name <- x[substr(names(x), 1, 4) == "name"] %>% unname()
    summaryResult <- summaryResult %>%
      dplyr::mutate(
        !!paste0("level", k) := rlang::parse_expr(collapse(level))
      ) %>%
      dplyr::select(-dplyr::all_of(level))
    if (length(name) > 0) {
      summaryResult <- summaryResult %>%
        dplyr::mutate(
          !!paste0("name", k) := rlang::parse_expr(collapse(name))
        ) %>%
        dplyr::select(-dplyr::all_of(name))
    }
  }
  join <- c(paste0("level", seq_along(wide)), paste0("name", seq_along(wide)))
  columnLabels <- summaryResult %>%
    dplyr::select(dplyr::any_of(join)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(column_name = paste0("cohort", dplyr::row_number()))
  summaryResult <- summaryResult %>%
    dplyr::inner_join(columnLabels, by = join) %>%
    dplyr::select(-dplyr::any_of(join)) %>%
    tidyr::pivot_wider(names_from = "column_name", values_from = "estimate")
  attr(summaryResult, "column_labels") <- columnLabels
  return(summaryResult)
}
arrangeLong <- function(summaryResult, long) {
  for (k in seq_along(long)) {
    x <- long[[k]]
    level <- x[substr(names(x), 1, 5) == "level"] %>% unname()
    name <- x[substr(names(x), 1, 4) == "name"] %>% unname()
    summaryResult <- summaryResult %>%
      dplyr::mutate(
        !!paste0("level", k) := rlang::parse_expr(collapse(level))
      ) %>%
      dplyr::select(-dplyr::all_of(level))
    if (length(name) > 0) {
      summaryResult <- summaryResult %>%
        # dplyr::mutate(
        #   !!paste0("name", k) := rlang::parse_expr(collapse(name))
        # ) %>%
        dplyr::select(-dplyr::all_of(name))
      # to be continued, not supported by the moment
    } else {
      summaryResult <- summaryResult %>%
        dplyr::rename(!!names(long)[k] := paste0("level", k))
    }
  }
  summaryResult <- summaryResult %>%
    dplyr::select(dplyr::all_of(c(
      names(long), attr(summaryResult, "column_labels")$column_name
    )))
}
collapse <- function(level) {
  paste0(
    "paste0(", paste0(".data$`", level, "`", collapse = ", \": \", "), ")"
  )
}
styleTable <- function(summaryTable, long, columnLabels, style) {
  # clean long data
  summaryTable <- cleanLongResult(summaryTable, long, style)
#   summaryTable <- cleanBorders(summaryTable)
#   summaryTable <- summaryTable %>%
#     tab_style(
#       style = list(cell_borders(
#         sides = "right", color = "#000000", weight = px(1)
#       )),
#       locations = list(cells_body(columns = "Format"))
#     )

  # clean wide data
  summaryTable <- summaryTable %>%
    addWideLabels(columnLabels, style)

  # fix columns widths
  summaryTable <- summaryTable %>%
    editWidth(wide, long)
    # cols_width(
    #   # num ~ px(150),
    #   # ends_with("r") ~ px(100),
    #   # starts_with("cohort") ~ px(200),
    #   Variable ~ px(250),
    #   Level ~ px(100),
    #   Format ~ px(250),
    #   everything() ~ px(200)
    # )

  return(summaryTable)
}

cleanResult <- function(summaryResult, cols) {
  for (col in cols) {
    x <- summaryResult[[col]]
    x[is.na(x)] <- ""
    x[x == dplyr::lag(x)] <- ""
    summaryResult[[col]] <- x
  }
  return(summaryResult)
}
cleanBorders <- function(gtTable, summaryResult) {
  id <- which(summaryResult$Variable == "")
  gtTable <- gtTable %>%
    tab_style(
      style = list(cell_borders(style = "hidden", sides = "top")),
      locations = list(cells_body(columns = "Variable", rows = id))
    )
}
getWideTibble <- function(columns, wide) {
  tib <- columns %>% dplyr::select("name")
  count <- 1
  for (k in seq_along(wide)) {
    tib <- tib %>%
      dplyr::mutate(!!paste0("sep_", k) := names(wide)[k])
    cols <- wide[[k]]
    if ("name" %in% names(cols)) {
      cols <- columns %>%
        dplyr::select("name", dplyr::all_of(unname(cols))) %>%
        tidyr::pivot_wider(
          names_from = unname(cols["name"]), values_from = unname(cols["level"])
        )
      for (i in 2:length(cols)) {
        tib <- tib %>%
          dplyr::mutate(!!paste0("cat_", count) := colnames(cols)[i]) %>%
          dplyr::left_join(
            cols %>%
              dplyr::select(
                "name", !!paste0("lab_", count) := colnames(cols)[i]
              ),
            by = "name"
          )
        count <- count + 1
      }
    } else {
      tib <- tib %>%
        dplyr::left_join(
          columns %>%
            dplyr::select(
              "name", !!paste0("lab_", count) := unname(cols["level"])
            ),
          by = "name"
        )
      count <- count + 1
    }
  }
  return(tib)
}
arrangeLong <- function(summaryResult, long, wideTibble) {
  for (k in seq_along(long)) {
    x <- long[[k]]
    if ("name" %in% names(x)) {
      newCol <- names(long)[k]
      name <- unname(x["name"])
      level <- unname(x["level"])
      newName <- summaryResult %>%
        dplyr::select(dplyr::all_of(unname(x))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(!!paste0("break_", newCol) := dplyr::if_else(
          is.na(.data[[level]]), as.character(NA), .data[[name]]
        )) %>%
        dplyr::mutate(!!newCol := dplyr::if_else(
          is.na(.data[[level]]), .data[[name]],
          paste0("     ", .data[[level]])
        ))
      summaryResult <- summaryResult %>%
        dplyr::left_join(newName, by = unname(x)) %>%
        dplyr::select(-dplyr::all_of(unname(x)))
    } else {
      summaryResult <- summaryResult %>%
        dplyr::rename(!!names(long)[k] := unname(x["level"]))
    }
  }
  summaryResult <- summaryResult %>%
    dplyr::select(
      dplyr::starts_with("break_"),
      dplyr::all_of(c(names(long), wideTibble$name))
    )
  return(summaryResult)
}
addBreaks <- function(summaryResult) {
  findOrderId <- function(summaryResult, lab, colNew) {
    min(summaryResult$order_id[summaryResult[[colNew]] == lab]) - 0.5
  }
  x <- colnames(summaryResult)
  x <- x[substr(x, 1, 6) == "break_"]
  for (col in x) {
    colNew <- substr(col, 7, nchar(col))
    labels <- summaryResult[[col]] %>% unique()
    labels <- labels[!is.na(labels)]
    summaryResult <- summaryResult %>%
      dplyr::mutate(order_id = dplyr::row_number())
    breakTibble <- dplyr::tibble(!!colNew := labels) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        order_id = findOrderId(summaryResult, .data[[colNew]], colNew)
      )
    summaryResult <- summaryResult %>%
      dplyr::union_all(breakTibble) %>%
      dplyr::arrange(.data$order_id) %>%
      dplyr::select(-"order_id")
  }
  return(summaryResult)
}
addWideLabels <- function(gtTable, wideTibble) {
  wideTibble[is.na(wideTibble)] <- "-"
  # style <- list(
  #   sep = list(gt::cell_fill(color = "#969696"), gt::cell_text(weight = "bold"))#c(background = "gray", join = TRUE),
  #   cat = c(background = "none", join = TRUE),
  #   lab = c(background = "none", join = TRUE)
  # )
  for (k in length(wideTibble):2) {
    x <- wideTibble[[k]]
    type <- substr(names(wideTibble)[k], 1, 3)
    ii <- 1
    spannerIds <- NULL
    for (i in unique(x)) {
      id <- bwlabel(as.numeric(x == i))
      for (j in seq_len(max(id))) {
        spannerId <- paste0(names(wideTibble)[k], "_", ii, "_", j)
        gtTable <- gtTable %>%
          gt::tab_spanner(
            label = i, columns = dplyr::all_of(wideTibble$name[id == j]),
            id = spannerId
          )
        spannerIds <- c(spannerIds, spannerId)
      }
      ii <- ii + 1
    }
    # if (type == "sep") {
    # } else if (type == "cat") {
    # } else if (type == "lab") {
    # }
    if (type == "sep") {
      gtTable <- gtTable %>%
        gt::tab_style(
          style = list(gt::cell_fill(color = "#c8c8c8"), gt::cell_text(weight = "bold")),
          locations = gt::cells_column_spanners(spanners = spannerIds)
        )
    } else if (type == "cat") {
      gtTable <- gtTable %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#c8c8c8"),
          locations = gt::cells_column_spanners(spanners = spannerIds)
        )
    }
  }
  return(gtTable)
}
bwlabel <- function(x) {
  runs <- rle(x)
  labels <- rep(0, length(x))
  count <- 1
  for (i in 1:length(runs$lengths)) {
    if (runs$values[i] == 1) {
      labels[sum(runs$lengths[1:i]) - runs$lengths[i] + 1:runs$lengths[i]] <- count
      count <- count + 1
    }
  }
  return(labels)
}
pivotSettings <- function(summaryResult, toWide) {
  order <- summaryResult %>%
    dplyr::select("variable") %>%
    dplyr::distinct() %>%
    dplyr::mutate(order = dplyr::row_number())
  summaryResult <- formatNumbers(
    summaryResult, decimals = c(default = 0), decimalMark = ".", bigMark = ","
  )
  summaryResult <- tidyEstimates(
    summaryResult,
    format = c(
      "N (%)" = "count (percentage%)", "mean (sd)",
      "median [min; q25 - q75; max]",
      "median [q25 - q75]", "[min - max]",
      "N" = "count"
    ),
    keepNotFromatted = TRUE
  )
  long = list(
    #"Variable" = c(name = "variable", level = "variable_level"),
    "Variable" = c(level = "variable"),
    "Level" = c(level = "variable_level"),
    "Format" = c(level = "format")
  )
  wide = list(
    "CDM name" = c(level = "cdm_name"),
    "Group" = c(level = "group"),
    "Strata" = c(level = "strata")
  )
  getColumns <- function(elements) {
    lapply(elements, unname) %>%
      unlist() %>%
      unname()
  }
  join <- getColumns(wide)
  columns <- summaryResult %>%
    dplyr::select(dplyr::all_of(join)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(name = paste0("cohort", dplyr::row_number()))
  wideTibble <- getWideTibble(columns, wide)
  summaryResult <- summaryResult %>%
    dplyr::left_join(columns, by = join) %>%
    dplyr::select("name", dplyr::all_of(getColumns(long)), "estimate") %>%
    tidyr::pivot_wider(names_from = "name", values_from = "estimate") %>%
    dplyr::left_join(order, by = "variable") %>%
    dplyr::arrange(.data$order, .data$variable_level) %>%
    dplyr::select(-"order")
  summaryResult <- arrangeLong(summaryResult, long, wideTibble)
  summaryResult <- addBreaks(summaryResult)

  gtTable <- summaryResult %>%
    gt() %>%
    tab_style(
      style = list(cell_borders(
        sides = "right", color = "#000000", weight = px(1)
      )),
      locations = list(cells_body(columns = "Format"))
    ) %>%
    addWideLabels(wideTibble)
  gtTable <- cleanBorders(gtTable, summaryResult) %>%
    cols_width(
      # num ~ px(150),
      # ends_with("r") ~ px(100),
      # starts_with("cohort") ~ px(200),
      Variable ~ px(250),
      Level ~ px(100),
      Format ~ px(250),
      everything() ~ px(200)
    )
  gtTable
}

