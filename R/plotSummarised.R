#' Summarise cohort timing
#'
#' @param result A cohort_overlap object.
#' @param percentage If TRUE the overlap plot will display percentages,
#' otherwise it will present counts.
#' @param subjects If TRUE the overlap plot will display number of subjects,
#' otherwise it will present number of records.
#' @param facetBy Names of columns in the result dataframe for faceting the
#' ggplot object.
#' @param cohortLabels A glue expression to identify each plotted cohort
#' overlap.
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#' }
#'
plotCohortOverlap <- function(result,
                              percentage = TRUE,
                              subjects = TRUE,
                              facetBy = NULL,
                              cohortLabels = "{cohort_name_reference}; {cohort_name_comparator}") {
  x <- result |>
    visOmopResults::splitAll() |>
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) |>
    dplyr::select(!dplyr::all_of(c("result_type", "package_name", "package_version",
                                   "estimate_type", "estimate_name"))) |>
    getTidyOverlap() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c("cdm_name", "cohort_name_reference", "cohort_name_comparator")),dplyr::desc)) |>
    dplyr::mutate(
      total = .data$reference + .data$comparator - .data$overlap,
      comparison_name = glue::glue(cohortLabels)
    )

  if (percentage) {
    x <- x |>
      dplyr::mutate(
        reference = .data$reference/.data$total * 100,
        comparator = .data$comparator/.data$total * 100,
        total = 100
      )
    x_breaks <- c(0, 25, 50, 75, 100)
    x_labels <- c("0%", "25%", "50%", "75%", "100%")
  } else {
    x_breaks <- round(c(0, max(x$total)/4, max(x$total)/2,
                        max(x$total)*3/4, max(x$total)))
    x_labels <- x_breaks
  }

  if (subjects) {
    x <- x |>
      dplyr::filter(.data$variable_name == "number subjects")
  } else {
    x <- x |>
      dplyr::filter(.data$variable_name == "number records")
  }

  x$y_pos = seq(0, nrow(x)-1, 1)

  gg <- x |>
    ggplot2::ggplot() +
    # x
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = .data$reference,
                                    ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35),
                       fill = "#669bbc", alpha = 0.5) +
    # y
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$total - .data$comparator,
                                    xmax = .data$total,
                                    ymin = .data$y_pos-0.35, ymax = .data$y_pos+0.35),
                       fill = "#cd5d67",
                       alpha = 0.4) +
    ggplot2::scale_y_continuous(breaks = x$y_pos, labels = x$comparison_name) +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, NA)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 11)) +
    ggplot2::geom_col(data = dplyr::tibble("reference" = 0, "comparator" = 0,
                                           to_fill = c("Cohort reference", "Cohort comparator", "Overlap")),
                      ggplot2::aes(x = .data$reference, y = .data$comparator, fill = .data$to_fill)) +
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
