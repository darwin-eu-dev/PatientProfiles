#' Plot cohort_overlap objects.
#'
#' @param result A cohort_overlap object.
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param strataName Names of the strata names to include.
#' @param strataLevel Names of the strata levels to include.
#' @param cdmName Names of the databases to include.
#' @param variableName Names of the variable names to include.
#' @param facetBy Names of columns in the cohort_overlap table for faceting the
#' ggplot object.
#' @param overlapLabel A glue expression to identify each plotted cohort
#' overlap.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#'
#' @return A ggplot2.
#' @export
#'
#' @examples
#' \donttest{
#' }
#'
plotCohortOverlap <- function(result,
                              cohortNameReference = NULL,
                              cohortNameComparator = NULL,
                              strataName = NULL,
                              strataLevel = NULL,
                              cdmName = NULL,
                              variableName = "number subjects",
                              facetBy = NULL,
                              overlapLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                              uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  checkmate::assertCharacter(strataName, null.ok = TRUE)
  checkmate::assertCharacter(strataLevel, null.ok = TRUE)
  checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertCharacter(variableName, null.ok = TRUE)
  checkmate::assertCharacter(facetBy, null.ok = TRUE)
  checkmate::assertCharacter(overlapLabel)
  checkmate::assertLogical(uniqueCombinations)

  # split table
  x <- result |>
    visOmopResults::splitGroup()

  # add default values
  # add default values
  selectors <- defaultColumnSelectors(
    x, list("cohort_name_reference" = cohortNameReference, "cohort_name_comparator" = cohortNameComparator,
            "strata_name" = strataName, "cdm_name" = cdmName, "variable_name" = variableName))
  cohortNameReference <- selectors$cohort_name_reference
  cohortNameComparator <- selectors$cohort_name_comparator
  strataName <- selectors$strata_name
  cdmName <- selectors$cdm_name
  if (is.null(strataLevel)) {
    strataLevel <- unique(x$strata_level[x$strata_name %in% strataName])
  }

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }
  x <- x |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$variable_name == .env$variableName) |>
    dplyr::filter(.data$strata_name %in% .env$strataName) |>
    dplyr::filter(.data$strata_level %in% .env$strataLevel) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    getTidyOverlap() |>
    dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    dplyr::arrange(dplyr::across(
      dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator")),
      dplyr::desc)) |>
    dplyr::mutate(
      total = .data$reference + .data$comparator - .data$overlap,
      reference = .data$reference/.data$total * 100,
      comparator = .data$comparator/.data$total * 100,
      total = 100,
      only_reference = .data$reference - .data$overlap,
      only_comparator = .data$comparator - .data$overlap,
      dplyr::across(
        dplyr::all_of(c("strata_name", "strata_level",
                        "cohort_name_reference", "cohort_name_comparator")),
        ~stringr::str_to_sentence(gsub("_", " ", gsub("&&&", "and", .x)))),
      comparison_name = glue::glue(.env$overlapLabel),
    )

  # vertical position
  # vertical position
  assingY <- c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator",
               "strata_name", "strata_level")
  if (!is.null(facetBy)) {
    x <- x |>
      tidyr::unite("facet_var",
                   dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
    assingY = assingY[!assingY %in% facetBy]

  }
    x <- x |>
      dplyr::distinct(dplyr::across(dplyr::all_of(assingY))) |>
      dplyr::arrange(dplyr::across(
        dplyr::all_of(assingY),
        dplyr::desc)) |>
      dplyr::mutate(y_pos = dplyr::row_number()) |>
      dplyr::left_join(x, by = assingY)


  x_breaks <- c(0, 25, 50, 75, 100)
  x_labels <- c("0%", "25%", "50%", "75%", "100%")

  gg <- x |>
    ggplot2::ggplot() +
    # x
    suppressWarnings(ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = .data$reference,
                                                     ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35,
                                                     label1 = .data$only_reference,
                                                     label2 = .data$only_comparator,
                                                     label3 = .data$overlap),
                                        fill = "#669bbc", alpha = 0.5)) +
    # y
    suppressWarnings(ggplot2::geom_rect(ggplot2::aes(xmin = .data$total - .data$comparator,
                                                     xmax = .data$total,
                                                     ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35,
                                                     label1 = .data$only_reference,
                                                     label2 = .data$only_comparator,
                                                     label3 = .data$overlap),
                                        fill = "#cd5d67",
                                        alpha = 0.4)) +
    ggplot2::scale_y_continuous(breaks = x$y_pos, labels = x$comparison_name) +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    suppressWarnings(ggplot2::geom_col(data = dplyr::tibble("reference" = 0, "comparator" = 0,
                                           to_fill = c("Cohort reference", "Cohort comparator", "Overlap")),
                      ggplot2::aes(x = .data$reference, y = .data$comparator, fill = .data$to_fill))) +
    ggplot2::scale_fill_manual(
      "Legend",
      values=c('#B6CDDE', '#E7BEC2', "#BBA0AE"),
      labels =  c("Cohort reference", "Cohort comparator", "Overlap")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      order = 3,
      override.aes = list(fill = c('#B6CDDE', '#E7BEC2', "#BBA0AE")))
    ) +
    ggplot2::ylab("") + ggplot2::xlab("")

  if (!is.null(facetBy)) {
    gg <- gg +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var))
  }

  return(gg)
}

#' Plot cohort_overlap objects.
#'
#' @param result A cohort_overlap object.
#' @param type Type of desired formatted table, possibilities are "boxplot" and
#' "density".
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param strataName Names of the strata names to include.
#' @param strataLevel Names of the strata levels to include.
#' @param cdmName Names of the databases to include.
#' @param facetBy Vector of column names  in the cohort_overlap table for faceting the
#' ggplot object.
#' @param color Vector of column names to distict by colors.
#' @param timingLabel A glue expression to identify each plotted cohort
#' overlap.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#'
#' @return A ggplot2.
#' @export
#'
#' @examples
#' \donttest{
#' }
#'
plotCohortTiming <- function(result,
                             type = "boxplot",
                             cohortNameReference = NULL,
                             cohortNameComparator = NULL,
                             strataName = NULL,
                             strataLevel = NULL,
                             cdmName = NULL,
                             facetBy = NULL,
                             color = c("cohort_name_reference", "cohort_name_comparator"),
                             timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                             uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(type, c("boxplot", "density"))
  checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  checkmate::assertCharacter(strataName, null.ok = TRUE)
  checkmate::assertCharacter(strataLevel, null.ok = TRUE)
  checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertCharacter(facetBy, null.ok = TRUE)
  checkmate::assertCharacter(color, null.ok = TRUE)
  checkmate::assertCharacter(timingLabel)
  checkmate::assertLogical(uniqueCombinations)

  # split table
  x <- result |>
    visOmopResults::splitGroup() |>
    visOmopResults::splitAdditional()

  # add default values
  selectors <- defaultColumnSelectors(
    x, list("cohort_name_reference" = cohortNameReference, "cohort_name_comparator" = cohortNameComparator,
            "strata_name" = strataName, "cdm_name" = cdmName))
  cohortNameReference <- selectors$cohort_name_reference
  cohortNameComparator <- selectors$cohort_name_comparator
  strataName <- selectors$strata_name
  cdmName <- selectors$cdm_name
  if (is.null(strataLevel)) {
      strataLevel <- unique(x$strata_level[x$strata_name %in% strataName])
  }

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  # prepare data
  x <- x |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$strata_name %in% .env$strataName) |>
    dplyr::filter(.data$strata_level %in% .env$strataLevel) |>
    dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
    dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator) |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value),
                  dplyr::across(
                    dplyr::all_of(c("strata_name", "strata_level",
                                    "cohort_name_reference", "cohort_name_comparator")),
                    ~stringr::str_to_sentence(gsub("_", " ", gsub("&&&", "and", .x))))) |>
    dplyr::arrange(dplyr::across(
      dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference",
                      "cohort_name_comparator")),
      dplyr::desc)) |>
    dplyr::mutate(timing_label = glue::glue(.env$timingLabel))

  # Plotting
  if (type == "boxplot") {
    # assert and get data
    x <- assertBoxplotEstimats(x)
    # color
    if (!is.null(color)) {
      gg <- x |>
        tidyr::unite("group",
                     dplyr::all_of(.env$color), remove = FALSE, sep = "; ") |>
        ggplot2::ggplot(ggplot2::aes(x = .data$timing_label, fill = .data$group))
    } else (
      gg <- x |>
        ggplot2::ggplot(ggplot2::aes(x = .data$timing_label))
    )
    # plot
    gg <- gg +
      ggplot2::geom_boxplot(
        ggplot2::aes(ymin = .data$q0, lower = .data$q25, middle = .data$q50,
                     upper = .data$q75, ymax = .data$q100),
        stat = "identity"
      ) +
      ggplot2::scale_x_discrete(labels = ggplot2::label_wrap_gen(15)) +
      ggplot2::ylab("Time (days)") +
      ggplot2::xlab("") +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    # facet
    if (!is.null(facetBy)) {
      gg$data <- gg$data |>
        tidyr::unite("facet_var",
                     dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
      gg <- gg +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet_var))
    }

  } else if (type == "density") {
    x <- assertDensityEstimates(x)

    # vertical position
    assingY <- c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator",
                 "strata_name", "strata_level")
    if (!is.null(facetBy)) {
      x <- x |>
        tidyr::unite("facet_var",
                     dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
      assingY = assingY[!assingY %in% facetBy]

    }
    x <- x |>
      dplyr::distinct(dplyr::across(dplyr::all_of(assingY))) |>
      dplyr::arrange(dplyr::across(
        dplyr::all_of(assingY),
        dplyr::desc)) |>
      dplyr::mutate(y_pos = dplyr::row_number()) |>
      dplyr::left_join(x, by = assingY)

    x <- x |>
      dplyr::mutate(height = max(.data$y)*1.05) |>
      dplyr::group_by(.data$plot_id) |>
      dplyr::mutate(
        y_pos = .data$y_pos*.data$height - .data$height,
        y = .data$y + .data$y_pos
      )

    # plot
    if (!is.null(color)) {
      x <- x |>
        tidyr::unite("color_var",
                     dplyr::all_of(.env$color), remove = FALSE, sep = "; ")
      gg <- x |>
        ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y,
                                     group = .data$plot_id,
                                     color = .data$color_var,
                                     fill = .data$color_var))
    } else {
      gg <- x |>
        ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y, group = .data$plot_id))
    }

    maxPlot = max(x$y)
    maxLabel = max(x$y_pos) + max(2*x$height/3)
    maxLim = max(maxPlot, maxLabel)

    gg <- gg +
      ggplot2::geom_line() +
      ggplot2::geom_polygon(alpha = 0.5) +
      ggplot2::geom_hline(yintercept = unique(x$y_pos), color = "gray") +
      ggplot2::geom_line(
        data = x |>
          dplyr::group_by(plot_id) |>
          dplyr::filter(abs(.data$q50 - .data$x) == min(abs(.data$q50 - .data$x))) |>
          tidyr::pivot_longer(cols = c("y", "y_pos"), values_to = "y") |>
          dplyr::ungroup() |>
          dplyr::select(-x) |>
          dplyr::rename("x" = "q50"),
        linewidth = 0.8
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#7f7f7f") +
      ggplot2::scale_y_continuous(
        breaks = x$y_pos + x$height/3,
        labels = x$timing_label,
        limits = c(0, maxLim)
      ) +
      ggplot2::xlab("Time (days)") +
      ggplot2::ylab("") +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::theme_light()

    # facet
    if (!is.null(facetBy)) {
      gg <- gg +
        ggplot2::facet_wrap(ggplot2::vars(.data$facet_var))
    }
  }

  return(gg)
}

getUniqueCombinations <- function(x, order) {
  dataCohortRef <-  unique(x$cohort_name_reference)
  order <- order[order %in% dataCohortRef]
  for (i in 2:length(order)) {
    x <- x |>
      dplyr::anti_join(
        x |>
          dplyr::filter(.data$cohort_name_reference == .env$order[i],
                        .data$cohort_name_comparator %in% .env$order[1:(i-1)]),
        by = colnames(x)
      )
  }
  return(x)
}

getTidyOverlap <- function(x) {
  cohort_counts <- x |>
    dplyr::filter(.data$cohort_name_reference == .data$cohort_name_comparator)
  byCol <- colnames(x)
  x <- x |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
    dplyr::mutate(variable_level = "overlap") |>
    tidyr::pivot_wider(names_from = c("variable_level"),
                       values_from = "estimate_value") |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "reference") |>
        tidyr::pivot_wider(names_from = c("variable_level"),
                           values_from = "estimate_value") |>
        dplyr::select(!"cohort_name_comparator"),
      by = byCol[! byCol %in% c("cohort_name_comparator", "variable_level", "estimate_value")]
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "comparator") |>
        tidyr::pivot_wider(names_from = c("variable_level"),
                           values_from = "estimate_value") |>
        dplyr::select(!"cohort_name_reference"),
      by = byCol[! byCol %in% c("cohort_name_reference", "variable_level", "estimate_value")]
    )
  return(x)
}

assertBoxplotEstimats <- function(x) {
  x <- x |>
    dplyr::mutate(
      estimate_name = dplyr::case_when(
        .data$estimate_name == "min" ~ "q0",
        .data$estimate_name == "max" ~ "q100",
        .data$estimate_name == "median" ~ "q50",
        .default = .data$estimate_name
      )
    ) |>
    dplyr::filter(.data$estimate_name %in% c("q0", "q25", "q50", "q75", "q100"))

  if (!all(c("q0", "q25", "q50", "q75", "q100") %in% unique(x$estimate_name))) {
    cli::cli_abort("To generate a boxplot cohort_timing must have the estimates:
                   q0 (or min), q25, q50 (or median), q75, and q100 (or max)")
  } else  {
    x <- x |>
      tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
  }
  return(x)
}

assertDensityEstimates <- function(x) {
  x <- x |>
    dplyr::mutate(
      estimate_name = dplyr::if_else(
        .data$estimate_name == "median", "q50", .data$estimate_name
      ),
      plot_id = paste0(.data$result_id, "; ", .data$cdm_name, "; ", .data$cohort_name_reference, "; ", .data$cohort_name_comparator, "; ",
                  .data$strata_name, "; ", .data$strata_level)
    ) |>
    dplyr::filter(.data$estimate_name %in% c("x", "y", "q50")) |>
    dplyr::select(dplyr::all_of(
      c("plot_id", "timing_label", "result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level",
        "estimate_name", "estimate_value")
    ))

  if (!all(c("x", "y") %in% unique(x$estimate_name))) {
    cli::cli_abort("To generate a density plot, cohort_timing must have the estimates: x, y.
                   Additionally, if q50 (or median) is in the data, it will be included in the plot.")
  }

  xMedian <- x |>
    dplyr::filter(.data$estimate_name == "q50") |>
    dplyr::select("plot_id", "q50" = "estimate_value") |>
    dplyr::distinct()

  x <- x |> dplyr::filter(.data$estimate_name != "q50")

  x <- lapply(as.list(unique(x$plot_id)), function(plot_id, data = x) {
    data <- data |>
      dplyr::filter(.data$plot_id == .env$plot_id)
    data <- data  |>
      dplyr::select(!dplyr::all_of(c("estimate_name", "estimate_value"))) |>
      dplyr::distinct() |>
      dplyr::left_join(
        dplyr::tibble(
          plot_id = plot_id,
          x = data$estimate_value[data$estimate_name == "x"],
          y = data$estimate_value[data$estimate_name == "y"]
        ),
        by = "plot_id"
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::left_join(xMedian, by = "plot_id")

  return(x)
}
