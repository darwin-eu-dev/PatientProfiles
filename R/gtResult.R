#' Create a gt table from a summary characteristics object
#'
#' @param summarisedResults Summary characteristics long table.
#' @param pivotWide
#' @param format
#' @param keepNotFormatted
#' @param decimals
#' @param decimalMark
#' @param bigMark
#' @param style
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
gtCharacteristics <- function(summarisedResults,
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
                              bigMark = ",",
                              style = list(defaultWidth = "200px", separator = c("Format"))) {
  gtResults(
    summarisedResults,
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
    format = format,
    keepNotFormatted = keepNotFormatted,
    decimals = decimals,
    decimalMark = decimalMark,
    bigMark = bigMark,
    style = style
  )
}

#' Give format to a summary object.
#'
#' @param summaryResult A SummarisedResult object.
#' @param long
#' @param wide
#' @param format
#' @param keepNotFormatted
#' @param eliminateUniqueLabels
#' @param decimals
#' @param decimalMark
#' @param bigMark
#' @param style
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
#'     format = c(
#'       "N (%)" = "count (percentage%)",
#'       "N" = "count",
#'       "median [Q25-Q75]" = "median [q25-q75]"
#'     ),
#'     decimals = c(count = 0),
#'     keepNotFormatted = FALSE
#'   )
#' }
#'
gtResult <- function(summaryResult,
                     long = list(
                       "Variable" = c(level = "variable", "clean"),
                       "Level" = c(level = "variable_level"),
                       "Format" = c(level = "format", "separator-right")
                     ),
                     wide = list(
                       "CDM Name" = c(level = "cdm_name"),
                       "Group" = c(level = c("group_name", "group_level")),
                       "Strata" = c(level = c("strata_name", "strata_level"))
                     ),
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
                     bigMark = ",",
                     style = list(
                       defaultWidth = "200px"
                     )) {
  # initial checks
  #checkInput(
  #  summaryResult = summaryResult, long = long, wide = wide, format = format,
  #  estimateColumn = estimateColumn, keepNotFormatted = keepNotFormatted,
  #  decimals = decimals, decimalMark = decimalMark, bigMark = bigMark
  #)

  # format decimals
  summaryTable <- formatNumbers(summaryResult, decimals, decimalMark, bigMark)

  # tidy estimates
  summaryTable <- tidyEstimates(summaryTable, format, keepNotFormatted)

  # select only needed columns
  summaryTable <- selectVariables(summaryTable, wide, long)

  # pivot wide
  summaryTable <- pivotWide(summaryTable, wide)
  columnLabels <- attr(summaryTable, "column_labels")
  attr(summaryTable, "column_labels") <- NULL

  # order data
  summaryTable <- orderData(summaryTable, summaryResult, columnLabels)

  # arrange long
  summaryTable <- arrangeLong(summaryTable, long, columnLabels)

  # create gt object
  summaryTable <- gt::gt(summaryTable) %>%
    styleTable(long, columnLabels, style)

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
pivotWide <- function(summaryResult, wide) {
  for (k in seq_along(wide)) {
    x <- wide[[k]]
    level <- x[substr(names(x), 1, 5) == "level"] %>% unname()
    name <- x[substr(names(x), 1, 4) == "name"] %>% unname()
    summaryResult <- summaryResult %>%
      dplyr::mutate(
        !!paste0("level", k) := !!rlang::parse_expr(collapse(level))
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
  for (k in seq_along(wide)) {
    name <- paste0("name", k)
    level <- paste0("level", k)
    if (name %in% colnames(summaryResult)) {
      nl <- summaryResult %>%
        dplyr::select(dplyr::all_of(c(name, level))) %>%
        dplyr::distinct()
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
            !!paste0("subtitle", k, "_", i) := labels[i],
            !!paste0("subtitlelevel", k, "_", i) := column
          )
      }
    }
  }

  join <- c(paste0("level", seq_along(wide)), paste0("name", seq_along(wide)))
  join <- join[join %in% colnames(summaryResult)]
  columnLabels <- summaryResult %>%
    dplyr::select(dplyr::all_of(join)) %>%
    dplyr::distinct()

    dplyr::mutate(column_name = paste0("cohort", dplyr::row_number()))
  summaryResult <- summaryResult %>%
    dplyr::inner_join(columnLabels, by = join) %>%
    dplyr::select(-dplyr::all_of(join)) %>%
    tidyr::pivot_wider(names_from = "column_name", values_from = "estimate")
  for (k in seq_along(wide)) {
    columnLabels <- columnLabels %>%
      dplyr::mutate(!!paste0("title", k) := names(wide)[k])
  }
  attr(summaryResult, "column_labels") <- columnLabels
  return(summaryResult)
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
styleTable <- function(summaryTable, long, columnLabels, style) {
  # style long data
  summaryTable <- styleLongResult(summaryTable, long)

  # style wide data
  summaryTable <- styleWideResult(summaryTable, columnLabels)

  # fix columns widths
  # summaryTable <- summaryTable %>%
  #   editWidth(wide, long)
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
styleWideResult <- function(summaryTable, columnLabels) {
  num <- gsub("title|name|level|column_name", "", colnames(columnLabels)) %>%
    as.numeric() %>%
    unique() %>%
    sort(decreasing = TRUE)
  for (id in num) {
    columnLabels
  }
  columnLabels[is.na(columnLabels)] <- "Overall"
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
    keepNotFormatted = TRUE
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

