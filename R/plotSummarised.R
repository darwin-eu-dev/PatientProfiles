#' Plot cohort_overlap objects.
#'
#' @param result A cohort_overlap object.
#' @param cohortNameReference Names of the reference cohorts to include.
#' @param cohortNameComparator Names of the comparator cohorts to include.
#' @param cdmName Name of the databases to include.
#' @param variableName Name of the variable names to include.
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
                              cdmName = NULL,
                              variableName = c("number subjects"),
                              facetBy = NULL,
                              overlapLabel = "{cohort_name_reference};
                              {cohort_name_comparator}",
                              uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(cohortNameReference, null.ok = TRUE)
  checkmate::assertCharacter(cohortNameComparator, null.ok = TRUE)
  checkmate::assertCharacter(cdmName, null.ok = TRUE)
  checkmate::assertCharacter(variableName, null.ok = TRUE)
  checkmate::assertCharacter(facetBy, null.ok = TRUE)
  checkmate::assertCharacter(overlapLabel, null.ok = TRUE)
  checkmate::assertLogical(uniqueCombinations)

  # split table
  x <- result |>
    visOmopResults::splitAll()

  # add default values
  cohortNameReference <- defaultColumnSelector(
    cohortNameReference,
    x$cohort_name_reference,
    "cohort_name_reference"
  )
  cohortNameComparator <- defaultColumnSelector(
    cohortNameComparator,
    x$cohort_name_comparator,
    "cohort_name_comparator"
  )
  variableName <- defaultColumnSelector(
    variableName,
    x$variable_name,
    "variable_name"
  )
  cdmName <- defaultColumnSelector(cdmName, x$cdm_name, "cdm_name")

  x <- result |>
    visOmopResults::splitAll()
  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }
  x <- x |>
    dplyr::filter(.data$cdm_name %in% .env$cdmName) |>
    dplyr::filter(.data$variable_name == .env$variableName) |>
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
      comparison_name = glue::glue(overlapLabel)
    )

  x_breaks <- c(0, 25, 50, 75, 100)
  x_labels <- c("0%", "25%", "50%", "75%", "100%")

  x$y_pos = seq(0, nrow(x)-1, 1)

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
    gg$data <- gg$data |>
      tidyr::unite("facet_var",
                   dplyr::all_of(.env$facetBy), remove = FALSE, sep = "; ")
    gg <- gg +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var))
  }

  return(gg)
}

getUniqueCombinations <- function(x, order) {
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
