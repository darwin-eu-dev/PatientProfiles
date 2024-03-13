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
                              strataName = "overall",
                              strataLevel = "overall",
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
      comparison_name = glue::glue(.env$overlapLabel)
    )
  if (!is.null(facetBy)) {
    x <- x |>
      tidyr::unite("facet_var",
                   dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
    x <- x |>
      dplyr::distinct(.data$comparison_name) |>
      dplyr::mutate(y_pos = dplyr::row_number()) |>
      dplyr::inner_join(x, by = c("comparison_name"))
  } else {
    x$y_pos = seq(0, nrow(x)-1, 1)
  }

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
#' @param type .
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
                             strataName = "overall",
                             strataLevel = "overall",
                             cdmName = NULL,
                             facetBy = NULL,
                             color = "timingLabel",
                             timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                             uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(type, c("boxplot"))
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
      dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator")),
      dplyr::desc)) |>
    dplyr::mutate(timingLabel = glue::glue(.env$timingLabel))

  # Plotting
  if (type == "boxplot") {
    # assert and get data
    x <- assertBoxplotEstimats(x)
    x <- x |>
      dplyr::filter(.data$estimate_name %in% c("q0", "q25", "q50", "q75", "q100")) |>
      tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
    # color
    if (!is.null(color)) {
      gg <- x |>
        tidyr::unite("group",
                     dplyr::all_of(.env$color), remove = FALSE, sep = "; ") |>
        ggplot2::ggplot(ggplot2::aes(x = .data$timingLabel, fill = .data$group))
    } else (
      gg <- x |>
        ggplot2::ggplot(ggplot2::aes(x = .data$timingLabel))
    )
    # plot
    gg <- gg +
      ggplot2::geom_boxplot(
        ggplot2::aes(ymin = .data$q0, lower = .data$q25, middle = .data$q50,
                     upper = .data$q75, ymax = .data$q100),
        stat = "identity"
      ) +
      ggplot2::ylab("Timing") +
      ggplot2::xlab("")
    # facet
    if (!is.null(facetBy)) {
      gg$data <- gg$data |>
        tidyr::unite("facet_var",
                     dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
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
    )
  if (!all(c("q0", "q25", "q50", "q75", "q100") %in% unique(x$estimate_name))) {
    cli::cli_abort("To generate a boxplot cohort_timing must have the estimates: q0 (or min), q25, q50 (or median), q75, and q100 (or max)")
  }
  return(x)
}
