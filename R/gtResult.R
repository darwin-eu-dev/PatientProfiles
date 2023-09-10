#' Create a gt table from a summarisedCharacteristics object.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param summarisedCharacteristics Summary characteristics long table.
#' @param pivotWide variables to pivot wide
#' @param format formats and labels to use
#' @param keepNotFormatted Wheather to keep not formated estimate types
#' @param decimals Decimals per estimate_type
#' @param decimalMark decimal mark
#' @param bigMark big mark
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
#'   gtCharacteristics()
#'}
gtCharacteristics <- function(summarisedCharacteristics,
                              pivotWide = c("CDM Name", "Group", "Strata"),
                              format = c(
                                "N (%)" = "count (percentage%)",
                                "median [min; q25 - q75; max]",
                                "mean (sd)",
                                "median [q25 - q75]",
                                "N" = "count"
                              ),
                              keepNotFormatted = TRUE,
                              decimals = c(default = 0),
                              decimalMark = ".",
                              bigMark = ",") {
  all <- list(
    "Variable" = c(level = "variable", "clean"),
    "Level" = c(level = "variable_level"),
    "Format" = c(level = "format"),
    "CDM Name" = c(level = "cdm_name"),
    "Group" = c(level = c("group_name", "group_level")),
    "Strata" = c(level = c("strata_name", "strata_level"))
  )
  wide <- all[names(all) %in% pivotWide]
  long <- all[!names(all) %in% pivotWide]
  long[[length(long)]] <- c(long[[length(long)]], "separator-right")
  gtResult(
    summarisedResult = summarisedCharacteristics, long = long, wide = wide,
    format = format, keepNotFormatted = keepNotFormatted, decimals = decimals,
    decimalMark = decimalMark, bigMark = bigMark
  )
}

#' Create a gt table from a summary object.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param summarisedResult A SummarisedResult object.
#' @param long List of variables and specification to long
#' @param wide List of variables and specification to wide
#' @param format formats and labels to use
#' @param keepNotFormatted Wheather to keep not formated estimate types
#' @param decimals Decimals per estimate_type
#' @param decimalMark decimal mark
#' @param bigMark big mark
#'
#' @return A formatted summarisedResult gt object.
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
#'   gtResult(
#'     long = list(
#'       "Variable" = c(level = "variable", "clean"),
#'       "Level" = c(level = "variable_level"),
#'       "Format" = c(level = "format", "separator-right")
#'     ),
#'     wide = list(
#'       "CDM Name" = c(level = "cdm_name"),
#'       "Group" = c(level = c("group_name", "group_level")),
#'       "Strata" = c(level = c("strata_name", "strata_level"))
#'     ),
#'     format = c(
#'       "N (%)" = "count (percentage%)",
#'       "N" = "count",
#'       "median [Q25-Q75]" = "median [q25-q75]"
#'     ),
#'     decimals = c(count = 0),
#'     keepNotFormatted = FALSE
#'    )
#' }
#'
gtResult <- function(summarisedResult,
                     long,
                     wide,
                     format = c(
                       "N (%)" = "count (percentage%)",
                       "median [min; q25 - q75; max]",
                       "mean (sd)",
                       "median [q25 - q75]",
                       "N" = "count"
                     ),
                     keepNotFormatted = TRUE,
                     decimals = c(default = 0),
                     decimalMark = ".",
                     bigMark = ",") {
  # initial checks
  #checkInput(
  #  summarisedResult = summarisedResult, long = long, wide = wide,
  #  format = format, keepNotFormatted = keepNotFormatted, decimals = decimals,
  #  decimalMark = decimalMark, bigMark = bigMark
  #)

  # format decimals
  summaryTable <- formatNumbers(summarisedResult, decimals, decimalMark, bigMark)

  # tidy estimates
  summaryTable <- tidyEstimates(summaryTable, format, keepNotFormatted)

  # select only needed columns
  summaryTable <- selectVariables(summaryTable, wide, long)

  # pivot wide
  summaryTable <- pivotWide(summaryTable, wide)
  columnLabels <- attr(summaryTable, "column_labels")
  attr(summaryTable, "column_labels") <- NULL

  # order data
  summaryTable <- orderData(summaryTable, summarisedResult, columnLabels)

  # arrange long
  summaryTable <- arrangeLong(summaryTable, long, columnLabels)

  # create gt object
  summaryTable <- gt::gt(summaryTable) %>%
    styleTable(long, columnLabels)

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
tidyEstimates <- function(summaryResult, format, keepNotFormatted) {
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
  if (keepNotFormatted == TRUE) {
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
pivotWide <- function(summaryTable, wide) {
  for (k in seq_along(wide)) {
    x <- wide[[k]]
    level <- x[substr(names(x), 1, 5) == "level"] %>% unname()
    name <- x[substr(names(x), 1, 4) == "name"] %>% unname()
    summaryTable <- summaryTable %>%
      dplyr::mutate(
        !!paste0("level", k) := !!rlang::parse_expr(collapse(level))
      ) %>%
      dplyr::select(-dplyr::all_of(level))
    if (length(name) > 0) {
      summaryTable <- summaryTable %>%
        dplyr::mutate(
          !!paste0("name", k) := !!rlang::parse_expr(collapse(name))
        ) %>%
        dplyr::select(-dplyr::all_of(name))
    }
  }
  cols <- colnames(summaryTable)
  cols <- cols[substr(cols, 1, 5) == "level" | substr(cols, 1, 4) == "name"]
  columnLabels <- summaryTable %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::distinct()
  for (k in seq_along(wide)) {
    name <- paste0("name", k)
    level <- paste0("level", k)
    if (name %in% colnames(summaryTable)) {
      nl <- columnLabels %>%
        dplyr::select(dplyr::all_of(c(name, level))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(!!paste0("title_", k) := names(wide)[k])
      xName <- strsplit(nl[[name]], " and ")
      xLevel <- strsplit(nl[[level]], " and ")
      labels <- unique(unlist(xName))
      for (i in seq_along(labels)) {
        column <- rep("Overall", length(xName))
        for (j in seq_along(xName)) {
          id <- which(xName[[j]] == labels[i])
          if (length(id) == 1) {
            column[j] <- xLevel[[j]][id]
          }
        }
        nl <- nl %>%
          dplyr::mutate(
            !!paste0("subtitle_", k, "_", i) := labels[i],
            !!paste0("subtitlelevels_", k, "_", i) := column
          )
      }
      columnLabels <- columnLabels %>%
        dplyr::inner_join(nl, by = c(name, level))
    } else {
      columnLabels <- columnLabels %>%
        dplyr::mutate(
          !!paste0("title_", k) := names(wide)[k],
          !!paste0("titlelevels_", k) := .data[[level]]
        )
    }

  }
  columnLabels  <- columnLabels %>%
    dplyr::mutate(column_name = paste0("cohort", dplyr::row_number()))
  summaryTable <- summaryTable %>%
    dplyr::inner_join(
      columnLabels %>%
        dplyr::select(dplyr::all_of(cols), "column_name"),
      by = cols
    ) %>%
    dplyr::select(-dplyr::all_of(cols)) %>%
    tidyr::pivot_wider(names_from = "column_name", values_from = "estimate")
  columnLabels <- columnLabels %>%
    dplyr::select(-dplyr::all_of(cols))
  attr(summaryTable, "column_labels") <- columnLabels
  return(summaryTable)
}
orderData <- function(summaryTable, summaryResult, columnLabels) {
  join <- colnames(summaryTable)[!(
    colnames(summaryTable) %in% c(columnLabels$column_name, "format")
  )]
  summaryResult %>%
    dplyr::select(dplyr::all_of(join)) %>%
    dplyr::distinct() %>%
    dplyr::left_join(summaryTable, by = join)
}
arrangeLong <- function(summaryResult, long, columnLabels) {
  for (k in seq_along(long)) {
    x <- long[[k]]
    level <- x[substr(names(x), 1, 5) == "level"] %>% unname()
    name <- x[substr(names(x), 1, 4) == "name"] %>% unname()
    summaryResult <- summaryResult %>%
      dplyr::mutate(
        !!paste0("level", k) := !!rlang::parse_expr(collapse(level))
      ) %>%
      dplyr::select(-dplyr::all_of(level))
    if (length(name) > 0) {
      summaryResult <- summaryResult %>%
        # dplyr::mutate(
        #   !!paste0("name", k) := rlang::parse_expr(collapse(name))
        # ) %>%
        dplyr::select(-dplyr::all_of(name))
      # to be continued, not supported for the moment
    } else {
      summaryResult <- summaryResult %>%
        dplyr::rename(!!names(long)[k] := paste0("level", k))
    }
  }
  summaryResult <- summaryResult %>%
    dplyr::select(dplyr::all_of(c(names(long), columnLabels$column_name)))
  return(summaryResult)
}
collapse <- function(level) {
  paste0(
    "paste0(", paste0(".data$`", level, "`", collapse = ", \": \", "), ")"
  )
}
styleTable <- function(summaryTable, long, columnLabels) {
  # style long data
  summaryTable <- styleLongResult(summaryTable, long)

  # style wide data
  summaryTable <- styleWideResult(summaryTable, columnLabels, long)

  # edit columns widths
  summaryTable <- editWidth(summaryTable, long)

  return(summaryTable)
}
styleLongResult <- function(summaryTable, long) {
  for (col in names(long)) {
    summaryTable$`_data`[[col]][summaryTable$`_data`[[col]] == "NA"] <- ""
  }
  for (k in seq_along(long)) {
    col <- names(long)[k]
    if ("clean" %in% long[[k]]) {
      x <- summaryTable$`_data`[[col]]
      id <- which(x == dplyr::lag(x))
      summaryTable$`_data`[[col]][id] <- ""
      summaryTable <- summaryTable %>%
        gt::tab_style(
          style = list(gt::cell_borders(style = "hidden", sides = "top")),
          locations = list(gt::cells_body(columns = col, rows = id))
        )
    }
    if ("separator-right" %in% long[[k]]) {
      summaryTable <- summaryTable %>%
        gt::tab_style(
          style = list(gt::cell_borders(sides = "right")),
          locations = list(gt::cells_body(columns = col))
        )
    }
    if ("separator-left" %in% long[[k]]) {
      summaryTable <- summaryTable %>%
        gt::tab_style(
          style = list(gt::cell_borders(sides = "left")),
          locations = list(gt::cells_body(columns = col))
        )
    }
  }
  return(summaryTable)
}
styleWideResult <- function(summaryTable, columnLabels, long) {
  columns <- colnames(columnLabels)
  columns <- columns[seq_len(length(columns) - 1)]
  # create spanners
  for (col in rev(columns)) {
    x <- columnLabels[[col]]
    ii <- 1
    for (i in unique(x)) {
      # id <- bwlabel(as.numeric(x == i))
      # for (j in seq_len(max(id))) {
      #   spannerId <- paste0(col, "_", ii)
      #   summaryTable <- summaryTable %>%
      #     gt::tab_spanner(
      #       label = i,
      #       columns = dplyr::all_of(columnLabels$column_name[id == j]),
      #       id = spannerId
      #     )
      # }
      summaryTable <- summaryTable %>%
        gt::tab_spanner(
          label = i,
          columns = dplyr::all_of(columnLabels$column_name[x == i]),
          id = paste0(col, "_", ii)
        )
      ii <- ii + 1
    }
  }
  # style spanners
  style <- list(
    "title" = list(
      gt::cell_fill(color = "#c8c8c8"), gt::cell_text(weight = "bold")
    ),
    "titlelevels" = list(
      gt::cell_fill(color = "#ffffff")
    ),
    "subtitle" = list(
      gt::cell_fill(color = "#e1e1e1")
    ),
    "subtitlelevels" = list(
      gt::cell_fill(color = "#ffffff")
    )
  )
  for (spanner in summaryTable$`_spanners`$spanner_id) {
    type <- strsplit(spanner, "_")[[1]][1]
    summaryTable <- summaryTable %>%
      gt::tab_style(
        style = style[[type]],
        locations = gt::cells_column_spanners(spanners = spanner)
      )
  }
  return(summaryTable)
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
editWidth <- function(summaryTable, long) {
  default <- "200px"
  widths <- as.list(rep(default, ncol(summaryTable$`_data`)))
  names(widths) <- colnames(summaryTable$`_data`)
  for (l in names(long)) {
    x <- long[[l]]
    if ("width" %in% names(x)) {
      widths[[l]] <- unname(x["width"])
    }
  }
  for (col in names(widths)) {
    summaryTable <- summaryTable %>%
      gt::cols_width(stats::as.formula(paste0(col, " ~ '", widths[[col]], "'")))
  }
  return(summaryTable)
}
