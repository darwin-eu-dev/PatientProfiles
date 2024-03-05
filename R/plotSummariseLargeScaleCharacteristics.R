# Copyright 2022 DARWIN EU (C)
#
# This file is part of PatientProfiles
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Summarise characteristics of individuals
#'
#' @param data output of summariseLargeScaleCharacteristics
#' @param cohort_name the cohort name you want to characterise
#' @param compare_cohorts Whether to compare cohort or not
#' @param cohort_2_name if is not NULL, need to provide cohort_2_name as second cohort to compare to
#' @param facet_vars column in data to facet by
#' @param color_vars column in data to color by
#' @param color_names A vector or pre-selected color
#' @param options Other plot options in a list
#' @return A ggplot
#' @export
plotSummariseLargeScaleCharacteristics <- function(data, cohort_name, compare_cohorts = FALSE,
                                                   cohort_2_name = NULL,
                                                   facet_vars = c("estimate_type"),
                                                   color_vars = c("variable_level", "group_level"),
                                                   color_names = NULL, options = list()) {
  if (nrow(data) == 0) {
    stop("Input data is empty. Please provide a non-empty data frame.")
  }

  if (!cohort_name %in% data$group_level) {
    stop("cohort_name not found in group_level column.")
  }

  if (!is.null(cohort_2_name) && !cohort_name %in% data$group_level) {
    stop("cohort_2_name not found in group_level column.")
  }

  if (!compare_cohorts && !is.null(cohort_2_name)) {
    warning("cohort_2_name is provided but compare_cohorts is FALSE. cohort_2_name will be ignored.")
  }

  if (compare_cohorts && is.null(cohort_2_name)) {
    stop("When compare_cohorts is TRUE, cohort_2_name must be provided.")
  }


  # Create interactions for color if there are multiple color vars
  if (length(color_vars) > 1) {
    color_interaction <- interaction(data[, color_vars], sep = ":")
    data <- data %>% dplyr::mutate(color_interaction = as.factor(.env$color_interaction))
    color_var <- "color_interaction"
  } else {
    color_var <- color_vars
  }

  # Ensure facet_vars is a character vector and create a new facet variable that combines them
  if (length(facet_vars) > 1) {
    data$facet_combined <- as.factor(interaction(data[, facet_vars], sep = ":"))
  } else {
    data$facet_combined <- facet_vars
  }

  if (!compare_cohorts) {
    dataFiltered <- data %>% dplyr::filter(.data$group_level == .env$cohort_name)
  } else {
    if (is.null(cohort_2_name)) {
      stop("When compare_cohorts is TRUE, cohort_2_name must be provided.")
    }
    dataFiltered <- data %>% dplyr::filter(.data$group_level %in% c(.env$cohort_name, .env$cohort_2_name))
  }

  # Enhance data with a column for faceting that combines facet variable names and values
  dataFiltered <- dataFiltered %>%
    dplyr::mutate(facet_label = paste(facet_vars, eval(parse(text = facet_vars)), sep = ": ")) %>%
    dplyr::mutate(facet_label = interaction(!!!rlang::syms(facet_vars), sep = ", "), .keep_all = TRUE)

  # Prepare for custom colors if provided
  custom_colors <- NULL
  if (!is.null(color_names) && length(color_names) == length(unique(dataFiltered[[color_var]]))) {
    custom_colors <- stats::setNames(color_names, unique(dataFiltered[[color_var]]))
  } else if (!is.null(color_names)) {
    stop("Length of color_names does not match the number of unique combinations in color_var.")
  }
  # Start constructing the plot
  p <- ggplot2::ggplot(dataFiltered, ggplot2::aes(x = "variable_name", y = "estimate_value", color = !!rlang::sym(color_var))) +
    ggplot2::geom_point() +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = "Characteristics Plot",
      x = "Characteristic",
      y = "Estimate Value",
      color = "Color"
    )
  # Apply custom or default colors
  if (!is.null(custom_colors)) {
    p <- p + ggplot2::scale_color_manual(values = custom_colors)
  }

  # Facet the plot using custom labels
  # Facet the plot: directly use facet_vars if provided
  if (!is.null(facet_vars) && length(facet_vars) > 0) {
    # Use facet_wrap with vars() for dynamic faceting based on provided variables
    facet_formula <- paste("ggplot2::vars(", paste(facet_vars, collapse = ", "), ")")
    p <- p + ggplot2::facet_wrap(eval(parse(text = facet_formula)), scales = "free_y")
  }

  # Apply additional options
  if (!is.null(options) && length(options) > 0) {
    for (option in options) {
      p <- p + option
    }
  }

  # Adjustments for comparing two cohorts
  if (compare_cohorts && !is.null(cohort_2_name)) {
    p <- p + ggplot2::geom_point(
      data = dataFiltered %>% dplyr::filter(.data$group_level == cohort_2_name),
      ggplot2::aes(x = "variable_name", y = "estimate_value", color = !!rlang::sym(color_var)),
      shape = 17
    ) # Different shape for second cohort
    p <- p + ggplot2::labs(title = paste("Comparison between Cohorts:", cohort_name, "and", cohort_2_name))
  }

  # Customize aesthetics
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text.x = ggplot2::element_text(size = 10, face = "bold"), # facet title
    panel.spacing = ggplot2::unit(1, "lines"), # Increase spacing between facets
    legend.position = "bottom"
  )

  return(p)
}
