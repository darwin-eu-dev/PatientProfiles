#' Plot the result of summariseCohortOverlap.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summariseCohortOverlap result.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by
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
                              facetVarX = "variable_name",
                              facetVarY = "strata_level",
                              colorVars = "variable_level",
                              overlapLabel = "{cohort_name_reference} &&& {cohort_name_comparator}",
                              uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_overlap")
  checkmate::assertCharacter(facetVarX, null.ok = TRUE)
  checkmate::assertCharacter(facetVarY, null.ok = TRUE)
  checkmate::assertCharacter(overlapLabel)
  checkmate::assertLogical(uniqueCombinations)




  # split table
  x <- result |>
    visOmopResults::tidy(splitStrata = FALSE) %>%
    dplyr::mutate(group_level = glue::glue(.env$overlapLabel))

  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  suppressMessages(data_to_plot <- result %>%
    dplyr::inner_join(x) %>%
    dplyr::filter(.data$estimate_type == "percentage") %>%
    dplyr::select(names(result)))

  return(
    plotCharacteristics(data_to_plot,
                        xAxis = "estimate_value",
                        yAxis = "group_level",
                        facetVarX = facetVarX,
                        facetVarY = facetVarY,
                        colorVars = colorVars,
                        plotStyle = "barplot")
    )
}

#' Plot summariseCohortTiming results.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param result A summariseCohortTiming result.
#' @param plotType Type of desired formatted table, possibilities are "boxplot" and
#' "density".
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars Column names to distinct by colors. default set to group_level
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
                             plotType = "boxplot",
                             facetVarX = "variable_name",
                             facetVarY = "group_level",
                             colorVars = "group_level",
                             timingLabel = "{cohort_name_reference} &&& {cohort_name_comparator}",
                             uniqueCombinations = TRUE) {
  # initial checks
  result <- omopgenerics::newSummarisedResult(result) |>
    dplyr::filter(.data$result_type == "cohort_timing")
  checkmate::assertChoice(plotType, c("boxplot", "density"))
  checkmate::assertCharacter(facetVarX, null.ok = TRUE)
  checkmate::assertCharacter(facetVarY, null.ok = TRUE)
  checkmate::assertCharacter(colorVars, null.ok = TRUE)
  checkmate::assertCharacter(timingLabel)
  checkmate::assertLogical(uniqueCombinations)
  if (plotType == "density" & !"density"%in% result$variable_name) {
    cli::cli_abort("Please provide a cohort timing summarised result with density estimates (use `density = TRUE` in summariseCohortTiming).")
  }




  # split table
  x <- result |> visOmopResults::tidy(splitStrata = FALSE) |>
    dplyr::mutate(group_level = glue::glue(.env$timingLabel))



  if (uniqueCombinations) {
    x <- x |>
      getUniqueCombinations(order = sort(unique(x$cohort_name_reference)))
  }

  suppressMessages(data_to_plot <- result %>%
    dplyr::inner_join(x) %>%
    dplyr::select(names(result)))



  # Plotting
  if (plotType == "boxplot") {
    return(
      gg <- plotCharacteristics(data_to_plot,
                          xAxis = "estimate_value",
                          yAxis = "group_level",
                          facetVarX = facetVarX,
                          facetVarY = facetVarY,
                          colorVars = colorVars,
                          plotStyle = "boxplot")
    )
    } else if (plotType == "density") {
      data_to_plot <- data_to_plot %>% dplyr::filter(.data$variable_name == "density")
      gg <- plotCharacteristics(data_to_plot,
                          xAxis = "estimate_value",
                          yAxis = "group_level",
                          facetVarX = facetVarX,
                          facetVarY = facetVarY,
                          colorVars = colorVars,
                          vertical_x = TRUE,
                          plotStyle = "density")

  }

  return(gg)
}

#' Plot summariseDemographics output.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param data output of summariseCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
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
                             facetVarX = "variable_name",
                             facetVarY = c("group_level", "strata_level"),
                             colorVars = "variable_level",
                             vertical_x = FALSE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
      c("summarised_demographics"))

  if (plotStyle == "barplot") {
    return(
      PatientProfiles::plotCharacteristics(
        data = data %>% dplyr::filter(.data$estimate_type == "percentage"),
        xAxis = xAxis,
        yAxis = yAxis,
        plotStyle = plotStyle,
        facetVarX = facetVarX,
        facetVarY = facetVarY,
        colorVars = colorVars,
        vertical_x = vertical_x
      )
    )
  }

  if (plotStyle == "boxplot") {
    return(
      PatientProfiles::plotCharacteristics(
        data = data,
        xAxis = xAxis,
        yAxis = yAxis,
        plotStyle = "boxplot",
        facetVarX = facetVarX,
        facetVarY = facetVarY,
        colorVars = colorVars,
        vertical_x = vertical_x
      )
    )
  }
}

#' Plot summariseCohortIntersect output.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param data output of summariseCohortIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
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
                                xAxis = "estimate_value",
                                yAxis = "variable_name",
                                plotStyle = "barplot",
                                facetVarX = "variable_name",
                                facetVarY = c("group_level", "strata_level"),
                                colorVars = "variable_level",
                                vertical_x = TRUE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
      c("summarised_cohort_intersect"))

  gg <- PatientProfiles::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    colorVars = colorVars,
    facetVarX = facetVarX,
    facetVarY = facetVarY,
    vertical_x = vertical_x
  )

  return(gg)
}

#' Plot summariseTableIntersect output.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param data output of summariseTableIntersect
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param plotStyle Now allows boxplot or barplot only.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.

plotTableIntersect <- function(data,
                               xAxis = "variable_name",
                               yAxis = "estimate_value",
                               plotStyle = "boxplot",
                               facetVarX = "variable_name",
                               facetVarY = c("group_level", "strata_level"),
                               colorVars = NULL,
                               vertical_x = TRUE) {
  # check input
  data <- omopgenerics::newSummarisedResult(data) |>
    dplyr::filter(.data$result_type %in%
      c("summarised_table_intersect"))

  gg <- PatientProfiles::plotCharacteristics(
    data = data,
    xAxis = xAxis,
    yAxis = yAxis,
    plotStyle = plotStyle,
    facetVarX = facetVarX,
    facetVarY = facetVarY,
    colorVars = colorVars,
    vertical_x = vertical_x
  )

  return(gg)
}

getUniqueCombinations <- function(x, order) {
  dataCohortRef <- unique(x$cohort_name_reference)
  order <- order[order %in% dataCohortRef]
  for (i in 2:length(order)) {
    x <- x |>
      dplyr::anti_join(
        x |>
          dplyr::filter(
            .data$cohort_name_reference == .env$order[i],
            .data$cohort_name_comparator %in% .env$order[1:(i - 1)]
          ),
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
    tidyr::pivot_wider(
      names_from = c("variable_level"),
      values_from = "estimate_value"
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "reference") |>
        tidyr::pivot_wider(
          names_from = c("variable_level"),
          values_from = "estimate_value"
        ) |>
        dplyr::select(!"cohort_name_comparator"),
      by = byCol[!byCol %in% c("cohort_name_comparator", "variable_level", "estimate_value")]
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "comparator") |>
        tidyr::pivot_wider(
          names_from = c("variable_level"),
          values_from = "estimate_value"
        ) |>
        dplyr::select(!"cohort_name_reference"),
      by = byCol[!byCol %in% c("cohort_name_reference", "variable_level", "estimate_value")]
    )
  return(x)
}

# assertBoxplotEstimats <- function(x) {
#   x <- x |>
#     dplyr::mutate(
#       estimate_name = dplyr::case_when(
#         .data$estimate_name == "min" ~ "q0",
#         .data$estimate_name == "max" ~ "q100",
#         .data$estimate_name == "median" ~ "q50",
#         .default = .data$estimate_name
#       )
#     ) |>
#     dplyr::filter(.data$estimate_name %in% c("q0", "q25", "q50", "q75", "q100"))
#
#   if (!all(c("q0", "q25", "q50", "q75", "q100") %in% unique(x$estimate_name))) {
#     cli::cli_abort("To generate a boxplot cohort_timing must have the estimates:
#                    q0 (or min), q25, q50 (or median), q75, and q100 (or max)")
#   } else {
#     x <- x |>
#       tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value")
#   }
#   return(x)
# }

assertDensityEstimates <- function(x) {
  x <- x |>
    dplyr::mutate(
      estimate_name = dplyr::if_else(
        .data$estimate_name == "median", "q50", .data$estimate_name
      ),
      plot_id = paste0(
        .data$result_id, "; ", .data$cdm_name, "; ", .data$cohort_name_reference, "; ", .data$cohort_name_comparator, "; ",
        .data$strata_name, "; ", .data$strata_level
      )
    ) |>
    dplyr::filter(.data$estimate_name %in% c("x", "y", "q50")) |>
    dplyr::select(dplyr::all_of(
      c(
        "plot_id", "timing_label", "result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level",
        "estimate_name", "estimate_value"
      )
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
    data <- data |>
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
