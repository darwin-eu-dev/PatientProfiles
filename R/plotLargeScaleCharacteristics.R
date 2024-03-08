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

#' plot large scale characteristics
#'
#' @param data output of summariseLargeScaleCharacteristics
#' @param xAxis what to plot on x axis, default as variable_name column. Has to be a column in data
#' @param yAxis what to plot on y axis, default as estimate_value column. Has to be a column in data
#' @param facetVars column in data to facet by
#' @param colorVars column in data to color by
#' @param facetOrder order of facet, make  sure multiple facets are separated by period and in the order provided in facetVars
#' @param colorNames A vector or pre-selected color
#' @param options Other plot options in a list
#' @return A ggplot
#' @export
plotLargeScaleCharacteristics <- function(data,
                                          xAxis = "variable_name",
                                          yAxis = "estimate_value",
                                          facetVars = NULL,
                                          colorVars = NULL,
                                          facetOrder = NULL,
                                          colorNames = NULL,
                                          options = list()) {
  return(plot(data,
    xAxis,
    yAxis,
    plotStyle = "scatterplot",
    facetVars,
    colorVars,
    facetOrder,
    colorNames,
    options
  ))
}

plot <- function(data,
                 xAxis = "variable_name",
                 yAxis = "estimate_value",
                 plotStyle = "scatterplot",
                 facetVars = NULL,
                 colorVars = NULL,
                 facetOrder = NULL,
                 colorNames = NULL,
                 options = list()) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(data, "summarised_result"))
  checkmate::assertTRUE(all(c(xAxis, yAxis, facetVars, colorVars) %in% colnames(data)))
  checkmate::assertVector(facetOrder, add = errorMessage, null.ok = TRUE)
  checkmate::assertList(options, add = errorMessage)



  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Empty Data Provided", subtitle = "No data available for plotting."))
  }


  # Create interactions for color if there are multiple color vars
  if (length(colorVars) > 1) {
    color_interaction <- interaction(data[, colorVars], sep = ":")
    data <- data %>% dplyr::mutate(color_interaction = as.factor(.env$color_interaction))
    color_var <- "color_interaction"
  } else {
    color_var <- colorVars
  }

  # Ensure facetVars is a character vector and create a new facet variable that combines them
  if (length(facetVars) > 1) {
    data$facet_combined <- as.factor(interaction(data[, facetVars], sep = "."))
  } else {
    data <- data %>% dplyr::mutate(facet_combined = .data[[facetVars]])
  }

  checkmate::assertTRUE(all(facetOrder %in% data$facet_combined), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)



  if (!is.null(facetOrder)) {
    data$facet_combined <- factor(data$facet_combined, levels = facetOrder)
    data$facet_combined <- droplevels(data$facet_combined)
    data <- data[!is.na(data$facet_combined), ] # drop the levels user did not specify! (Albert request)
  }



  dataFiltered <- data %>%
    dplyr::filter(
      .data$estimate_type %in% c("percentage", "proportion", "numeric", "integer")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(colorVars), as.factor),
      estimate_value = as.numeric(.data$estimate_value)
    ) # Ensure factors for color interaction




  # Prepare for custom colors if provided
  custom_colors <- NULL
  if (!is.null(colorNames) && length(colorNames) == length(unique(dataFiltered[[color_var]]))) {
    custom_colors <- stats::setNames(colorNames, unique(dataFiltered[[color_var]]))
  } else if (!is.null(colorNames)) {
    stop("Length of colorNames does not match the number of unique combinations in color_var.")
  }
  # Start constructing the plot


  if (plotStyle == "scatterplot") {
    p <- dataFiltered %>% ggplot2::ggplot(ggplot2::aes_string(
      x = xAxis,
      y = yAxis,
      color = color_var
    )) +
      ggplot2::geom_point()
  } else if (plotStyle == "barplot") {
    p <- dataFiltered %>%
      ggplot2::ggplot(ggplot2::aes_string(
        y = xAxis, # switched places with xAxis for a horizontal plot
        x = yAxis, # switched places with yAxis for a horizontal plot
        fill = color_var
      )) +
      ggplot2::geom_col() +
      ggplot2::coord_flip()
  }


  if (!is.null(custom_colors) & plotStyle == "scatterplot") {
    p <- p + ggplot2::scale_color_manual(values = custom_colors)
  }

  # Apply custom or default colors
  if (!is.null(custom_colors) & plotStyle == "barplot") {
    p <- p + ggplot2::scale_color_manual(scale_fill_manual = custom_colors)
  }


  xAxis_is_string <- is.character(dataFiltered[[xAxis]]) || is.factor(dataFiltered[[xAxis]])


  if (!is.null(facetVars) && length(facetVars) > 0) {
    if (all(unique(data$estimate_type) == "percentage")) {
      facet_formula <- "~ facet_combined"
      p <- p + ggplot2::facet_wrap(eval(parse(text = facet_formula)), scales = "free_y")
      if (xAxis_is_string) {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
      }
    } else if (plotStyle == "barplot") {
      split_data <- split(dataFiltered, f = dataFiltered[facetVars])

      plots <- lapply(names(split_data), function(data_name) {
        data <- split_data[[data_name]]

        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = yAxis, y = xAxis, fill = color_var)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::theme_light() +
          ggplot2::labs(title = paste("Facet:", data_name), ) +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 2, face = "bold"), # facet title
            panel.spacing = ggplot2::unit(1, "lines"),
            legend.position = "none",
            plot.title = ggplot2::element_text(size = 8)
          )

        if (xAxis_is_string) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
        }

        plot
      })


      # Combine the plots
      p <- ggpubr::ggarrange(
        plotlist = plots,
        common.legend = FALSE, legend = "bottom"
      )
    } else {
      split_data <- split(dataFiltered, f = dataFiltered[facetVars])

      plots <- lapply(names(split_data), function(data_name) {
        data <- split_data[[data_name]]

        plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = yAxis, y = xAxis, color = color_var)) +
          ggplot2::geom_point() +
          ggplot2::theme_light() +
          ggplot2::labs(title = paste("Facet:", data_name), ) +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 2, face = "bold"), # facet title
            panel.spacing = ggplot2::unit(1, "lines"),
            legend.position = "none",
            plot.title = ggplot2::element_text(size = 8)
          )

        if (xAxis_is_string) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
        }

        plot
      })


      # Combine the plots
      p <- ggpubr::ggarrange(
        plotlist = plots,
        common.legend = TRUE, legend = "bottom"
      )
    }
  }






  # Apply additional options
  if (!is.null(options) && length(options) > 0) {
    for (option in options) {
      p <- p + option
    }
  }



  return(p)
}
