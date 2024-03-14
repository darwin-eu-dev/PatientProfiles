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
#' @param vertical_x whether to display x axis string vertically
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
                                          vertical_x = FALSE,
                                          options = list()) {
  return(plotfunction(data,
    xAxis,
    yAxis,
    plotStyle = "scatterplot",
    facetVars,
    colorVars,
    facetOrder,
    colorNames,
    vertical_x,
    options
  ))
}

plotfunction <- function(data,
                         xAxis = "variable_name",
                         yAxis = "estimate_value",
                         plotStyle = "scatterplot",
                         facetVars = NULL,
                         colorVars = NULL,
                         facetOrder = NULL,
                         colorNames = NULL,
                         vertical_x = FALSE,
                         options = list()) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(data, "summarised_result"))
  checkmate::assertTRUE(all(c(xAxis, yAxis, facetVars, colorVars) %in% colnames(data)))
  checkmate::assertVector(facetOrder, add = errorMessage, null.ok = TRUE)
  checkmate::assertList(options, add = errorMessage, null.ok = TRUE)



  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Empty Data Provided", subtitle = "No data available for plotting."))
  }


  # Create interactions for color if there are multiple color vars
  if (length(colorVars) > 1) {
    color_interaction <- interaction(data[, colorVars], sep = ":")
    data <- data %>% dplyr::mutate(color_combined = as.factor(.env$color_interaction))
  } else if (!is.null(colorVars)) {
    data <- data %>% dplyr::mutate(color_combined = .data[[colorVars]])
  }

  # Ensure facetVars is a character vector and create a new facet variable that combines them
  if (length(facetVars) > 1) {
    data$facet_combined <- as.factor(interaction(data[, facetVars], sep = "."))
  } else if (!is.null(facetVars)) {
    data <- data %>% dplyr::mutate(facet_combined = .data[[facetVars]])
  }



  if (!is.null(facetOrder)) {
    if (!is.null(facetVars)) {
      checkmate::assertTRUE(all(facetOrder %in% data$facet_combined), add = errorMessage)
      data$facet_combined <- factor(data$facet_combined, levels = facetOrder)
      data$facet_combined <- droplevels(data$facet_combined)
      data <- data[!is.na(data$facet_combined), ]
    } else {
      errorMessage <- c(errorMessage, "please provide facetVars before specify facetOrder")
    } # drop the levels user did not specify! (Albert request)
  }

  checkmate::reportAssertions(collection = errorMessage)



  df_dates <- data %>% dplyr::filter(.data$estimate_type == "date")
  df_non_dates <- data %>%
    dplyr::filter(.data$estimate_type != "date") %>%
    dplyr::mutate(estimate_value = round(as.numeric(.data$estimate_value), 2))

  # Prepare for custom colors if provided
  custom_colors <- NULL
  if (!is.null(colorNames) && length(colorNames) == length(unique(df_non_dates$color_combined))) {
    custom_colors <- stats::setNames(colorNames, unique(df_non_dates$color_combined))
  } else if (!is.null(colorNames)) {
    stop("Length of colorNames does not match the number of unique combinations in
          colorVars column provided")
  }


  # Start constructing the plot
  if (plotStyle == "scatterplot") {
    p <- df_non_dates %>% ggplot2::ggplot(ggplot2::aes_string(
      x = xAxis,
      y = yAxis,
      color = "color_combined"
    )) +
      ggplot2::geom_point()
  } else if (plotStyle == "barplot") {
    df_non_dates <- df_non_dates %>% dplyr::filter(.data$estimate_name == "percentage")
    p <- df_non_dates %>%
      ggplot2::ggplot(ggplot2::aes_string(
        y = xAxis, # switched places with xAxis for a horizontal plot
        x = yAxis, # switched places with yAxis for a horizontal plot
        fill = "color_combined"
      )) +
      ggplot2::geom_col() +
      ggplot2::coord_flip()
  } else if (plotStyle == "boxplot") {
    df_non_dates <- df_non_dates %>%
      dplyr::filter(.data$estimate_name %in% c("q05", "q25", "median", "q75", "q95", "min", "max")) %>%
      dplyr::mutate(estimate_value = as.numeric(.data$estimate_value))

    df_dates <- df_dates %>%
      dplyr::filter(.data$estimate_name %in% c("q05", "q25", "median", "q75", "q95", "min", "max")) %>%
      dplyr::mutate(estimate_value = as.Date(.data$estimate_value))



    df_non_dates_wide <- df_non_dates %>%
      tidyr::pivot_wider(
        id_cols = c(
          "variable_name", "group_level",
          if ("facet_combined" %in% names(df_non_dates)) "facet_combined" else NULL,
          if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
        ),
        names_from = "estimate_name", values_from = "estimate_value"
      )

    df_dates_wide <- df_dates %>%
      tidyr::pivot_wider(
        id_cols = c(
          "variable_name", "group_level",
          if ("facet_combined" %in% names(df_non_dates)) "facet_combined" else NULL,
          if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
        ),
        names_from = "estimate_name", values_from = "estimate_value"
      )

    df_non_dates_wide$group_identifier <- interaction(df_non_dates_wide$color_combined, df_non_dates_wide$facet_combined)
    if (any(table(df_non_dates_wide$group_identifier) < 1)) {
      stop("One or more groups have less than one data point. Cannot create boxplot.")
    }
    df_dates_wide$group_identifier <- interaction(df_dates_wide$color_combined, df_dates_wide$facet_combined)
    if (any(table(df_dates_wide$group_identifier) < 1)) {
      stop("One or more groups have less than one data point. Cannot create boxplot.")
    }


    p_non_dates <- df_non_dates_wide %>% ggplot2::ggplot(
      ggplot2::aes(x = .data$variable_name) # ggplot2::aes(x = interaction(variable_name, group_level))
    )

    if ("color_combined" %in% names(df_non_dates_wide)) {
      p_non_dates <- p_non_dates + ggplot2::aes(color = .data$color_combined)
    }

    p_non_dates <- p_non_dates + ggplot2::geom_boxplot(
      ggplot2::aes(
        group = .data$group_identifier,
        lower = .data$q25,
        upper = .data$q75,
        middle = .data$median,
        ymin = .data$min,
        ymax = .data$max
      ),
      stat = "identity"
    ) +
      ggplot2::labs(
        title = "Non-Date Data", x = "Variable and Group Level",
        y = "Quantile Values"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    p_dates <- df_dates_wide %>% ggplot2::ggplot(
      ggplot2::aes(x = .data$variable_name) # ggplot2::aes(x = interaction(variable_name, group_level))
    )

    if ("color_combined" %in% names(df_dates_wide)) {
      p_dates <- p_dates + ggplot2::aes(color = .data$color_combined)
    }
    p_dates <- p_dates +
      ggplot2::geom_boxplot(
        ggplot2::aes(
          group = .data$group_identifier,
          lower = .data$q25,
          upper = .data$q75,
          middle = .data$median,
          ymin = .data$min,
          ymax = .data$max
        ),
        stat = "identity"
      ) +
      ggplot2::labs(
        title = "Date Data", x = "Variable and Group Level",
        y = "Quantile Values"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }


  if (!is.null(custom_colors) & plotStyle == "scatterplot") {
    p <- p + ggplot2::scale_color_manual(values = custom_colors)
  }

  # Apply custom or default colors
  if (!is.null(custom_colors) & plotStyle == "barplot") {
    p <- p + ggplot2::scale_color_manual(scale_fill_manual = custom_colors)
  }




  if (!is.null(facetVars) && length(facetVars) > 0) {
    if (plotStyle == "scatterplot") {
      p <- p + ggplot2::facet_wrap(eval(parse(text = "~ facet_combined")),
        scales = "free_y"
      )
      if (vertical_x) {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ))
      }
    } else if (plotStyle == "boxplot") {
      p_dates <- p_dates + ggplot2::facet_wrap(eval(parse(text = "~ facet_combined")),
        scales = "free"
      )
      if (vertical_x) {
        p_dates <- p_dates + ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ))
      }
      p_non_dates <- p_non_dates + ggplot2::facet_wrap(eval(parse(text = "~ facet_combined")),
        scales = "free"
      )
      if (vertical_x) {
        p_non_dates <- p_non_dates + ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ))
      }
      p <- ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
    } else if (plotStyle == "barplot") {
      split_data <- split(df_non_dates, f = df_non_dates[facetVars])

      plots <- lapply(names(split_data), function(data_name) {
        data <- split_data[[data_name]]

        plot <- ggplot2::ggplot(data, ggplot2::aes_string(
          x = yAxis, y = xAxis,
          fill = "color_combined"
        )) +
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

        if (vertical_x) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
        }

        plot
      })


      # Combine the plots
      p <- ggpubr::ggarrange(
        plotlist = plots,
        common.legend = FALSE, legend = "bottom"
      )
    }
    # else {
    #   split_data <- split(df_non_dates, f = dataFiltered[facetVars])
    #
    #   plots <- lapply(names(split_data), function(data_name) {
    #     data <- split_data[[data_name]]
    #
    #     plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = yAxis, y = xAxis, color = color_var)) +
    #       ggplot2::geom_point() +
    #       ggplot2::theme_light() +
    #       ggplot2::labs(title = paste("Facet:", data_name), ) +
    #       ggplot2::theme(
    #         strip.text.x = ggplot2::element_text(size = 2, face = "bold"), # facet title
    #         panel.spacing = ggplot2::unit(1, "lines"),
    #         legend.position = "none",
    #         plot.title = ggplot2::element_text(size = 8)
    #       )
    #
    #     if (vertical_x) {
    #       plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
    #     }
    #
    #     plot
    #   })
    #
    #
    #   # Combine the plots
    #   p <- ggpubr::ggarrange(
    #     plotlist = plots,
    #     common.legend = TRUE, legend = "bottom"
    #   )
    # }
  }






  # Apply additional options
  if (!is.null(options) && length(options) > 0) {
    for (option in options) {
      p <- p + option
    }
  }



  return(p)
}
