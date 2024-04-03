#' Plot the result of summariseCohortOverlap.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summariseCohortOverlap result.
#' @param facetBy Names of columns in the result table for faceting the
#' ggplot object.
#' @param overlapLabel A glue expression to identify each plotted cohort
#' overlap.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' overlap <- summariseCohortOverlap(cdm$cohort2)
#' plotCohortOverlap(overlap)
#' }
#'
plotCohortOverlap <- function(result,
                              facetBy = "variable_name",
                              overlapLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                              uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(facetBy, null.ok = TRUE)
  checkmate::assertCharacter(overlapLabel)
  checkmate::assertLogical(uniqueCombinations)

  # split table
  x <- result |>
    visOmopResults::tidy(splitStrata = FALSE, pivotEstimatesBy = c("variable_level", "estimate_name"))

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  x <- x |>
    dplyr::arrange(dplyr::across(
      dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level")),
      dplyr::desc)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("strata_name", "strata_level",
                        "cohort_name_reference", "cohort_name_comparator")),
        ~stringr::str_to_sentence(gsub("_", " ", gsub("&&&", "and", .x)))),
      reference = paste0(.data$only_in_reference_count, " (", round(.data$only_in_reference_percentage, 2), "%)"),
      comparator = paste0(.data$only_in_comparator_count, " (", round(.data$only_in_comparator_percentage, 2), "%)"),
      overlap = paste0(.data$overlap_count, " (", round(.data$overlap_percentage, 2), "%)"),
      comparison_name = glue::glue(.env$overlapLabel),
    )

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
    # reference
    suppressWarnings(ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = .data$only_in_reference_percentage,
                                                     ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35,
                                                     label1 = .data$reference,
                                                     label2 = .data$comparator,
                                                     label3 = .data$overlap,
                                                     label4 = .data$variable_name),
                                        fill = "#B6CDDE")) +
    # overlap
    suppressWarnings(ggplot2::geom_rect(ggplot2::aes(xmin = .data$only_in_reference_percentage,
                                                     xmax = .data$only_in_reference_percentage + .data$overlap_percentage,
                                                     ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35,
                                                     label1 = .data$reference,
                                                     label2 = .data$comparator,
                                                     label3 = .data$overlap,
                                                     label4 = .data$variable_name),
                                        fill = "#BBA0AE")) +
    # comparator
    suppressWarnings(ggplot2::geom_rect(ggplot2::aes(xmin = 100 - .data$only_in_comparator_percentage,
                                                     xmax = 100,
                                                     ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35,
                                                     label1 = .data$reference,
                                                     label2 = .data$comparator,
                                                     label3 = .data$overlap,
                                                     label4 = .data$variable_name),
                                        fill = "#E7BEC2")) +
    ggplot2::scale_y_continuous(breaks = x$y_pos, labels = x$comparison_name) +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    suppressWarnings(ggplot2::geom_col(data = dplyr::tibble("only_in_reference_percentage" = 0, "only_in_comparator_percentage" = 0,
                                           to_fill = c("Cohort reference", "Cohort comparator", "Overlap")),
                      ggplot2::aes(x = .data$only_in_reference_percentage, y = .data$only_in_comparator_percentage, fill = .data$to_fill))) +
    ggplot2::scale_fill_manual(
      "Legend",
      values=c('#B6CDDE', '#E7BEC2', "#BBA0AE"),
      labels =  c("Cohort reference", "Cohort comparator", "Overlap")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      order = 3,
      override.aes = list(fill = c('#B6CDDE', '#E7BEC2', "#BBA0AE")))
    ) +
    ggplot2::ylab("") +
      ggplot2::xlab("")

  if (!is.null(facetBy)) {
    gg <- gg +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var))
  }

  return(gg)
}

#' Plot summariseCohortTiming results.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summariseCohortTiming result.
#' @param type Type of desired formatted table, possibilities are "boxplot" and
#' "density".
#' @param facetBy Vector of column names  in the cohort_overlap table for faceting the
#' ggplot object.
#' @param color Vector of column names to distinct by colors.
#' @param timingLabel A glue expression to identify each plotted cohort
#' overlap.
#' @param uniqueCombinations If TRUE, only unique combinations of reference and
#' comparator plots will be plotted.
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' timing <- summariseCohortTiming(cdm$cohort2)
#' plotCohortTiming(timing)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotCohortTiming <- function(result,
                             type = "boxplot",
                             facetBy = NULL,
                             color = c("cohort_name_reference", "cohort_name_comparator"),
                             timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                             uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(type, c("boxplot", "density"))
  checkmate::assertCharacter(facetBy, null.ok = TRUE)
  checkmate::assertCharacter(color, null.ok = TRUE)
  checkmate::assertCharacter(timingLabel)
  checkmate::assertLogical(uniqueCombinations)
  if (type == "density" & !"density"%in% result$variable_name) {
    cli::cli_abort("Please provide a cohort timing summarised result with density estimates (use `density = TRUE` in summariseCohortTiming).")
  }

  # split table
  x <- result |>
    visOmopResults::splitGroup() |>
    visOmopResults::splitAdditional()

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  # prepare data
  x <- x |>
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
          dplyr::group_by(.data$plot_id) |>
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

#' Plot summariseDemographics output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVars column in data to facet by.
#' @param colorVars column in data to color by.
#' @param facetOrder order of facet, make sure multiple facets are separated by period and in the order provided in facetVars.
#' @param colorNames A vector or pre-selected color.
#' @param vertical_x whether to display x axis string vertically.
#' @param options Other plot options in a list.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' results <- summariseDemographics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
#' )
#' graph <- plotDemographics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotDemographics <- function(data,
                             xAxis = "variable_name",
                             yAxis = "estimate_value",
                             plotStyle = "barplot",
                             facetVars = NULL,
                             colorVars = "variable_level",
                             facetOrder = NULL,
                             colorNames = NULL,
                             vertical_x = FALSE,
                             options = list()) {

  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_demographics"))

  gg <- PatientProfiles::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    facetVars = facetVars,
    colorVars = colorVars,
    facetOrder = facetOrder,
    colorNames = colorNames,
    vertical_x = vertical_x,
    options = options
  )

  return(gg)
}

#' Plot summariseCohortIntersect output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseCohortIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVars column in data to facet by.
#' @param colorVars column in data to color by.
#' @param facetOrder order of facet, make sure multiple facets are separated by period and in the order provided in facetVars.
#' @param colorNames A vector or pre-selected color.
#' @param vertical_x whether to display x axis string vertically.
#' @param options Other plot options in a list.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' results <- summariseCohortIntersect(
#'   cohort = cdm$cohort1,
#'   cohortIntersect = list(
#'     "Medications in the prior year" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, -1)
#'     )
#'   )
#' )
#' graph <- plotCohortIntersect(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
plotCohortIntersect <- function(data,
                               xAxis = "variable_name",
                               yAxis = "estimate_value",
                               plotStyle = "barplot",
                               facetVars = NULL,
                               colorVars = NULL,
                               facetOrder = NULL,
                               colorNames = NULL,
                               vertical_x = FALSE,
                               options = list()) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_cohort_intersect"))

  gg <- PatientProfiles::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    facetVars = facetVars,
    colorVars = colorVars,
    facetOrder = facetOrder,
    colorNames = colorNames,
    vertical_x = vertical_x,
    options = options
  )

  return(gg)
}

#' Plot summariseTableIntersect output.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseTableIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVars column in data to facet by.
#' @param colorVars column in data to color by.
#' @param facetOrder order of facet, make sure multiple facets are separated by period and in the order provided in facetVars.
#' @param colorNames A vector or pre-selected color.
#' @param vertical_x whether to display x axis string vertically.
#' @param options Other plot options in a list.
#' @return A ggplot.

plotTableIntersect <- function(data,
                               xAxis = "variable_name",
                               yAxis = "estimate_value",
                               plotStyle = "boxplot",
                               facetVars = NULL,
                               colorVars = NULL,
                               facetOrder = NULL,
                               colorNames = NULL,
                               vertical_x = FALSE,
                               options = list()) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
                    c("summarised_table_intersect"))

  gg <- PatientProfiles::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    facetVars = facetVars,
    colorVars = colorVars,
    facetOrder = facetOrder,
    colorNames = colorNames,
    vertical_x = vertical_x,
    options = options
  )

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
