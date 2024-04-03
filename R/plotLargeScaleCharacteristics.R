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

#' create a ggplot from the output of summariseLargeScaleCharacteristics.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data output of summariseLargeScaleCharacteristics.
#' @param xAxis what to plot on x axis, default as variable_name column.
#' Has to be a column in data.
#' @param yAxis what to plot on y axis, default as estimate_value column.
#' Has to be a column in data. One of the xAxis or yAxis has to be estimate_value.
#' @param facetVarX column in data to facet by on horizontal axis
#' @param facetVarY column in data to facet by on vertical axis
#' @param colorVars column in data to color by.
#' @param vertical_x whether to display x axis string vertically.
#' @return A ggplot.
#' @export
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#'
#' concept <- dplyr::tibble(
#'   concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
#'   domain_id = NA_character_,
#'   vocabulary_id = NA_character_,
#'   concept_class_id = NA_character_,
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01")
#' ) %>%
#'   dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#' cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' results <- cdm$cohort2 %>%
#'   summariseLargeScaleCharacteristics(
#'     episodeInWindow = c("condition_occurrence"),
#'     minimumFrequency = 0
#'   )
#' graphs <- plotLargeScaleCharacteristics(results)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
plotLargeScaleCharacteristics <- function(data,
                                          xAxis = "variable_name",
                                          yAxis = "estimate_value",
                                          facetVarX = c("variable_name"),
                                          facetVarY = c("group_level", "strata_level", "estimate_name"),
                                          colorVars = "variable_level",
                                          vertical_x = FALSE) {
  return(plotfunction(data,
    xAxis,
    yAxis,
    plotStyle = "scatterplot",
    facetVarX,
    facetVarY,
    colorVars,
    vertical_x = vertical_x
  ))
}

plotfunction <- function(data,
                         xAxis = "variable_name",
                         yAxis = "estimate_value",
                         plotStyle = "scatterplot",
                         facetVarX = "variable_name",
                         facetVarY = c("group_level", "strata_level"),
                         colorVars = "variable_level",
                         vertical_x = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(data, "summarised_result"))
  all_vars <- c(xAxis, yAxis, facetVarX, facetVarY, colorVars)
  checkmate::assertTRUE(all(all_vars[!is.null(all_vars)] %in% colnames(data)))
  if (plotStyle == "density" & xAxis != "estimate_value") {
    stop(sprintf("If plotStyle is set to 'density', xAxis must be 'estimate_value'."))
  }

  checkmate::assertVector(facetVarX, add = errorMessage, null.ok = TRUE)
  checkmate::assertVector(facetVarY, add = errorMessage, null.ok = TRUE)
  # checkmate::assertList(options, add = errorMessage, null.ok = TRUE)


  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Empty Data Provided", subtitle = "No data available for plotting."))
  }


  # Create interactions for color if there are multiple color vars
  # if (length(colorVars) > 1) {
  #   color_interaction <- interaction(data[, colorVars], sep = ":")
  #   data <- data %>% dplyr::mutate(color_combined = as.factor(.env$color_interaction))
  # } else if (!is.null(colorVars)) {
  #   data <- data %>% dplyr::mutate(color_combined = .data[[colorVars]])
  # }

  # Ensure facetVars is a character vector and create a new facet variable that combines them
  # if (length(facetVars) > 1) {
  #   data$facet_combined <- as.factor(interaction(data[, facetVars], sep = "."))
  # } else if (!is.null(facetVars)) {
  #   data <- data %>% dplyr::mutate(facet_combined = .data[[facetVars]])
  # }

  if (plotStyle == "boxplot") {
    if (!all(c("q25", "median", "q75", "min", "max") %in% data$estimate_name)) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Data Provided",
            subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
          )
      )
    }
  }
  data <- data %>%
    dplyr::mutate(color_combined = construct_color_variable(data, colorVars))

  if (is.null(facetVarX)) {
    facetVarX <- "variable_name"
    warning("facetVarX cannot be NULL, set to variable_name")
  }

  if (is.null(facetVarY)) {
    facetVarY <- c("group_level", "strata_level")
    warning("facetVarY cannot be NULL, set to group_level and strata_level")
  }

  data <- data %>%
    dplyr::mutate(
      facet_combined_x = construct_variable(data, facetVarX),
      facet_combined_y = construct_variable(data, facetVarY)
    )

  # if (!is.null(facetOrder)) {
  #   if (!is.null(facetVars)) {
  #     checkmate::assertTRUE(all(facetOrder %in% data$facet_combined), add = errorMessage)
  #     data$facet_combined <- factor(data$facet_combined, levels = facetOrder)
  #     data$facet_combined <- droplevels(data$facet_combined)
  #     data <- data[!is.na(data$facet_combined), ]
  #   } else {
  #     errorMessage <- c(errorMessage, "please provide facetVars before specify facetOrder")
  #   } # drop the levels user did not specify! (Albert request)
  # }


  checkmate::assertTRUE(any(xAxis == "estimate_value", yAxis == "estimate_value"), add = errorMessage)


  checkmate::reportAssertions(collection = errorMessage)



  df_dates <- data %>% dplyr::filter(.data$estimate_type == "date")
  if (plotStyle != "density") {
    df_non_dates <- data %>%
      dplyr::filter(!(.data$estimate_type %in% c("date", "logical"))) %>%
      dplyr::mutate(estimate_value = round(as.numeric(.data$estimate_value), 2))
    if (nrow(df_non_dates) > 0) {
      df_non_dates <- df_non_dates %>%
        dplyr::mutate(
          estimate_value =
            dplyr::if_else(.data$estimate_name == "percentage",
              .data$estimate_value / 100,
              .data$estimate_value
            )
        )
    }
  } else {
    df_non_dates <- data %>%
      dplyr::filter(!(.data$estimate_type %in% c("date", "logical")))
  }



  # # Prepare for custom colors if provided
  # custom_colors <- NULL
  # if (!is.null(colorNames) && length(colorNames) == length(unique(df_non_dates$color_combined))) {
  #   custom_colors <- stats::setNames(colorNames, unique(df_non_dates$color_combined))
  # } else if (!is.null(colorNames)) {
  #   stop("Length of colorNames does not match the number of unique combinations in
  #         colorVars column provided")
  # }


  # Start constructing the plot
  if (plotStyle == "scatterplot") {
    if (nrow(df_non_dates) > 0) {
      df_percent <- df_non_dates %>% dplyr::filter(.data$estimate_name == "percentage")
      df_non_percent <- df_non_dates %>% dplyr::filter(.data$estimate_name != "percentage")

      make_plot <- function(data, is_percent = FALSE) {
        if ("color_combined" %in% names(data)) {
          plot <- data %>% ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis),
              color = .data$color_combined
            ))
          plot <- plot + ggplot2::labs(color = "Color")
        } else {
          plot <- data %>% ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis)
            ))
        }

        if (is_percent) {
          if (xAxis == "estimate_value") {
            plot <- plot + ggplot2::scale_x_continuous(
              labels = scales::percent_format(accuracy = 1)
            )
          } else if (yAxis == "estiamte_value") {
            plot <- plot + ggplot2::scale_y_continuous(
              labels = scales::percent_format(accuracy = 1)
            )
          }
        }

        return(plot)
      }

      p_percent <- if (nrow(df_percent) > 0) {
        make_plot(df_percent, TRUE)
      } else {
        NULL
      }
      p_non_percent <- if (nrow(df_non_percent) > 0) {
        make_plot(df_non_percent, FALSE)
      } else {
        NULL
      }
    } else {
      p_percent <- p_non_percent <- NULL
    }
  } else if (plotStyle == "barplot" || plotStyle == "density") {
    if (nrow(df_non_dates) > 0) {
      # Separate data based on 'estimate_name'
      df_percent <- df_non_dates %>% dplyr::filter(.data$estimate_name == "percentage")
      df_non_percent <- df_non_dates %>% dplyr::filter(.data$estimate_name != "percentage")

      # Function to create bar plots
      create_bar_plot <- function(data, plotStyle, is_percent = FALSE) {
        if (plotStyle == "barplot") {
          if ("color_combined" %in% names(data)) {
            plot <- data %>% ggplot2::ggplot(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis),
              fill = .data$color_combined
            )) +
              ggplot2::geom_col() +
              ggplot2::labs(fill = "Color")
          } else {
            plot <- ggplot2::ggplot(data, ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis)
            )) +
              ggplot2::geom_col()
          }


          # Apply percent formatting if data is 'percentage' and for the correct axis
          if (is_percent) {
            if (xAxis == "estimate_value") {
              plot <- plot + ggplot2::scale_x_continuous(
                labels = scales::percent_format(accuracy = 1)
              )
            } else if (yAxis == "estimate_value") {
              plot <- plot + ggplot2::scale_y_continuous(
                labels = scales::percent_format(accuracy = 1)
              )
            }
          }
        } else if (plotStyle == "density") {
          data <- data %>%
            dplyr::filter(.data$variable_name == "density") %>%
            dplyr::mutate(estimate_value = as.numeric(.data$estimate_value))
          group_columns <- data %>%
            dplyr::select(-c(
              "estimate_value", "estimate_name", "variable_level",
              if ("facet_combined_x" %in% names(df_non_dates)) "facet_combined_x" else NULL,
              if ("facet_combined_y" %in% names(df_non_dates)) "facet_combined_y" else NULL,
              if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
            )) %>%
            dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) %>%
            dplyr::select(dplyr::where(~.)) %>%
            names()

          data$group_identifier <- interaction(data %>%
            dplyr::select(dplyr::all_of(group_columns)))

          density_data_wide <- data %>%
            dplyr::mutate(estimate_value = as.list(.data$estimate_value)) %>%
            tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") %>%
            tidyr::unnest(dplyr::everything())

          if ("color_combined" %in% names(density_data_wide)) {
            plot <- density_data_wide %>% ggplot2::ggplot() +
              ggplot2::geom_density(
                ggplot2::aes(
                  x = data$x,
                  y = .data$y,
                  group = .data$group_identifier,
                  fill = .data$color_combined
                ),
                stat = "identity"
              ) + # Density plot
              ggplot2::labs(fill = "Color")
          } else {
            plot <- density_data_wide %>% ggplot2::ggplot() +
              ggplot2::geom_density(
                ggplot2::aes(
                  x = .data$x,
                  y = .data$y,
                  group = .data$group_identifier
                ),
                stat = "identity"
              ) + # Density plot
              ggplot2::labs(fill = "Color")
          }
        }

        return(plot)
      }

      if (plotStyle == "barplot") {
        # Create plots
        p_percent <- if (nrow(df_percent) > 0) {
          create_bar_plot(df_percent,
            plotStyle = "barplot",
            is_percent = TRUE
          )
        } else {
          NULL
        }
        p_non_percent <- if (nrow(df_non_percent) > 0) {
          create_bar_plot(df_non_percent,
            plotStyle = "barplot",
            is_percent = FALSE
          )
        } else {
          NULL
        }
      } else if (plotStyle == "density") {
        p_percent <- NULL
        p_non_percent <- if (nrow(df_non_percent) > 0) {
          create_bar_plot(df_non_percent,
            is_percent = FALSE,
            plotStyle = "density"
          )
        }
      }
    }
  } else if (plotStyle == "boxplot") {
    if (nrow(df_non_dates) > 0) {
      df_non_dates <- df_non_dates %>%
        dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) %>%
        dplyr::mutate(
          estimate_value = as.numeric(.data$estimate_value),
          estimate_type = "numeric"
        )
      non_numeric_cols <- df_non_dates %>%
        dplyr::select(-c(
          "estimate_value", "estimate_name",
          if ("facet_combined_x" %in% names(df_non_dates)) "facet_combined_x" else NULL,
          if ("facet_combined_y" %in% names(df_non_dates)) "facet_combined_y" else NULL,
          if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
        )) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) %>%
        dplyr::select(dplyr::where(~.)) %>%
        names()

      df_non_dates_wide <- df_non_dates %>%
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(colnames(
            df_non_dates %>%
              dplyr::select(-c("estimate_name", "estimate_value"))
          )),
          names_from = "estimate_name",
          values_from = "estimate_value"
        )


      if(length(non_numeric_cols) > 0){
      df_non_dates_wide$group_identifier <- interaction(df_non_dates_wide %>%
        dplyr::select(dplyr::all_of(non_numeric_cols)))} else{
          df_non_dates_wide$group_identifier <- "overall"
        }
    }

    if (nrow(df_dates) > 0) {
      df_dates <- df_dates %>%
        dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) %>%
        dplyr::mutate(estimate_value = as.Date(.data$estimate_value))

      df_dates_wide <- df_dates %>%
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(colnames(df_dates %>%
            dplyr::select(-c(
              "estimate_name",
              "estimate_value"
            )))),
          names_from = "estimate_name", values_from = "estimate_value"
        )
      if(length(non_numeric_cols) > 0){
      df_dates_wide$group_identifier <- interaction(df_dates_wide %>%
        dplyr::select(
          dplyr::all_of(non_numeric_cols)
        ))} else {
          df_dates_wide$group_identifier <- "overall"
        }
    }



    # Check if the dataframe has rows to plot
    if (nrow(df_non_dates) > 0) {
      xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)
      p_non_dates <- df_non_dates_wide %>% ggplot2::ggplot(
        ggplot2::aes(x = .data[[xcol]])) + ggplot2::labs(
          title = "Non-Date Data",
          x = "Variable and Group Level",
          y = "Quantile Values"
        )
      # +
      # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

      if ("color_combined" %in% names(df_non_dates_wide)) {
        if (!all(is.na(df_non_dates_wide$color_combined))) {
          p_non_dates <- p_non_dates + ggplot2::aes(color = .data$color_combined)
        }
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
        )

      # Determine if the plot should be horizontal or vertical based on 'estimate_value'
      if (xAxis == "estimate_value") {
        # Horizontal plot
        p_non_dates <- p_non_dates +
          ggplot2::coord_flip()
      }
    } else {
      # Setup for empty data
      p_non_dates <- NULL
    }


    if (nrow(df_dates) > 0) {
      xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)

      p_dates <- df_dates_wide %>% ggplot2::ggplot(
        ggplot2::aes(x = .data[[xcol]])) +
        ggplot2::labs(
          title = "Date Data",
          x = "Variable and Group Level",
          y = "Quantile Values"
        )

      if ("color_combined" %in% names(df_dates_wide)) {
        if (!all(is.na(df_dates_wide$color_combined))) {
          p_dates <- p_dates + ggplot2::aes(color = .data$color_combined)
        }
      }

      p_dates <- p_dates + ggplot2::geom_boxplot(
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
        )
      # Determine if the plot should be horizontal or vertical based on 'estimate_value'
      if (xAxis == "estimate_value") {
        # Horizontal plot
        p_dates <- p_dates +
          ggplot2::coord_flip()
      }
    } else {
      p_dates <- NULL
    }
  }

  #
  #   if (!is.null(custom_colors) & plotStyle == "scatterplot") {
  #     p <- p + ggplot2::scale_color_manual(values = custom_colors)
  #   }
  #
  #   # Apply custom or default colors
  #   if (!is.null(custom_colors) & plotStyle == "barplot") {
  #     p <- p + ggplot2::scale_color_manual(scale_fill_manual = custom_colors)
  #   }
  #



  if (suppressWarnings(!is.null(data$facet_combined_x) || !is.null(data$facet_combined_y))) {
    if (plotStyle == "scatterplot") {
      # Apply facet grid if either plot exists
      if (!is.null(p_percent) || !is.null(p_non_percent)) {
        if (!is.null(p_percent)) {
          theme_modification <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            )
          )

          facet_x_exists <- "facet_combined_x" %in% names(df_percent)
          facet_y_exists <- "facet_combined_y" %in% names(df_percent)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )
          p_percent <- p_percent + ggplot2::facet_grid(
            rows = facet_formula,
            scales = "free"
          )
          if (vertical_x) {
            p_percent <- p_percent + theme_modification
          }
        }

        if (!is.null(p_non_percent)) {
          facet_x_exists <- "facet_combined_x" %in% names(df_non_percent)
          facet_y_exists <- "facet_combined_y" %in% names(df_non_percent)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )
          p_non_percent <- p_non_percent + ggplot2::facet_grid(
            rows = facet_formula,
            scales = "free"
          )
          if (vertical_x) {
            p_non_percent <- p_non_percent + theme_modification
          }
        }
      }

      # Combine the plots or select the appropriate one
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Scatterplot only supported numeric values"
          )
      }
    } else if (plotStyle == "boxplot") {
      if (!is.null(p_dates)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_dates)
        facet_y_exists <- "facet_combined_y" %in% names(df_dates)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_dates <- p_dates +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_dates <- p_dates + ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ))
        }
      }
      if (!is.null(p_non_dates)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_non_dates)
        facet_y_exists <- "facet_combined_y" %in% names(df_non_dates)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_non_dates <- p_non_dates +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_non_dates <- p_non_dates + ggplot2::theme(
            axis.text.x =
              ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              )
          )
        }
      }


      p <- if (!is.null(p_dates) && !is.null(p_non_dates)) {
        ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
      } else if (!is.null(p_dates)) {
        p_dates
      } else if (!is.null(p_non_dates)) {
        p_non_dates
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Data Provided",
            subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
          )
      }
    } else if (plotStyle == "barplot" || plotStyle == "density") {
      if (!is.null(p_percent)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_percent)
        facet_y_exists <- "facet_combined_y" %in% names(df_percent)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_percent <- p_percent +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")

        if (vertical_x) {
          p_percent <- p_percent + ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ))
        }
      }

      if (!is.null(p_non_percent)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_non_percent)
        facet_y_exists <- "facet_combined_y" %in% names(df_non_percent)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )


        p_non_percent <- p_non_percent +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
      }


      # Combine the plots
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Barplot only supported numeric values"
          )
      }
    }
  } else {
    if (plotStyle == "barplot" || plotStyle == "density") {
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        if (vertical_x) {
          p_percent <- p_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        if (vertical_x) {
          p_percent <- p_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        p_percent
      } else if (!is.null(p_non_percent)) {
        if (vertical_x) {
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Barplot only supported numeric values"
          )
      }
    } else if (plotStyle == "boxplot") {
      if (!is.null(p_dates) || !is.null(p_non_dates)) {
        if (!is.null(p_dates) && is.null(p_non_dates)) {
          if (vertical_x) {
            p_dates <- p_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- p_dates
        } else if (is.null(p_dates) && !is.null(p_non_dates)) {
          if (vertical_x) {
            p_non_dates <- p_non_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- p_non_dates
        } else {
          if (vertical_x) {
            p_dates <- p_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
            p_non_dates <- p_non_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
        }
      }
    } else if (plotStyle == "scatterplot") {
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Scatterplot only supported numeric values"
          )
      }
    }
  }

  # Apply additional options
  # if (!is.null(options) && length(options) > 0) {
  #   for (option in options) {
  #     p <- p + option
  #   }
  # }

  return(p)
}

construct_variable <- function(data, facet_vars) {
  if (!is.null(facet_vars) && length(facet_vars) > 1) {
    unique_val_vars <- sapply(facet_vars, function(var) {
      dplyr::n_distinct(data[[var]], na.rm = TRUE) > 1
    })

    valid_vars <- facet_vars[unique_val_vars]

    if (length(valid_vars) > 1) {
      return(as.factor(interaction(data %>% dplyr::select(dplyr::all_of(valid_vars)), sep = ".")))
    } else if (length(valid_vars) == 1) {
      return(as.factor(data[[valid_vars]]))
    }
  } else if (!is.null(facet_vars) && length(facet_vars) == 1) {
    if (dplyr::n_distinct(data[[facet_vars]], na.rm = TRUE) > 1) {
      return(as.factor(data[[facet_vars]]))
    }
  }
  return(NULL)
}

construct_color_variable <- function(data, color_vars) {
  if (!is.null(color_vars) && length(color_vars) >= 1) {
    combined_factor <- interaction(dplyr::select(
      data,
      dplyr::all_of(color_vars)
    ), sep = ".")
    return(as.factor(combined_factor))
  }
  return(NULL)
}
