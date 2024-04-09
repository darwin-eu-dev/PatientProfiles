#' Summarise standardised mean difference
#'
#' @param data  summarised characteristics data
#' @param group  the column to use as group to compute SMD, now only supports
#' two unique values in this column. Default as "group_level"
#' @param variables column with variables to compute SMD for. Default as
#' variable_name
#' @param weight the weight column name in your data
#' @return A tibble with SMD computed
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' result <- summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   tableIntersect = list(
#'     tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#'   )
#' ) %>% dplyr::bind_rows(summariseCharacteristics(
#'   cohort = cdm$cohort2, tableIntersect = list(
#'     tableName = "visit_occurrence", value = "count", window = c(-365, -1)
#'   )
#' ))
#' compute_smd(result, group = "group_level")
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
compute_smd <- function(data,
                        group = "group_level",
                        variables = "variable_name",
                        weight = NULL) {
  if (is.null(weight)) {
    data <- data %>% dplyr::mutate(weight = 1)
  } else {
    data <- data %>% dplyr::rename("weight" = dplyr::all_of(weight))
  }

  smd_results <-  dplyr::tibble()
  unique_vars <- unique(data$variable_name)
  data <- data %>% visOmopResults::splitAdditional()

  entries_to_remove <- c(
    "Number records", "Number subjects",
    "Cohort start date", "Cohort end date"
  )

  unique_vars <- unique_vars[!unique_vars %in% entries_to_remove]
  for (variable in unique_vars) {
    var_data <- data %>% dplyr::filter(.data$variable_name == variable)

    if (all(c("mean", "sd") %in% var_data$estimate_name)) {
      # Check group count and handle error for continuous variables
      if (length(unique(var_data[[group]])) > 2) {
        stop("Number of labels in group column different from 2 for continuous variable: ", variable)
      }
      results <-  dplyr::tibble()

      continuous_results <- compute_smd_continuous_pairs(
        var_data %>%
          dplyr::distinct(),
        variable,
        group, weight,
        results = results
      )
      smd_results <- dplyr::bind_rows(smd_results, continuous_results)
    } else if (any(var_data$estimate_name == "percentage")) {
      results <- dplyr::tibble()
      cat_results <- asmdCategorical(
        var_data %>%
          dplyr::filter(.data$estimate_type == "percentage") %>%
          dplyr::distinct(),
        variable = variable, group = group, weight = weight,
        results = results
      )
      smd_results <- dplyr::bind_rows(smd_results, cat_results)
    }
  }

  return(smd_results)
}

compute_smd_continuous_pairs <- function(x, variable, group, weight, results) {
  groups <- unique(x[[group]])

  # Find columns with more than two levels excluding the estimate value column
  columns_to_process <- names(x)[sapply(names(x), function(col_name) {
    # Exclude specific columns from the analysis
    if (col_name %in% c(
      group, weight, "estimate_value", "variable_name",
      "estimate_name", "estimate_type"
    )) {
      FALSE
    } else {
      any(sapply(split(x[[col_name]], x[[group]]), function(subgroup) {
        length(unique(subgroup)) > 1
      }))
    }
  })]


  if (length(columns_to_process) == 0) {
    # No additional columns, proceed with just the group levels
    results <- results %>% dplyr::bind_rows(
      bind_smd_results(
        x,
        group,
        weight,
        variable
      )
    )
  } else {
    # Calculate SMD for each combination of subgroups within the variable_name group
    for (col in columns_to_process) {
      unique_values <- unique(x[[col]])

      # Process each unique value in the additional columns
      for (val in unique_values) {
        sub_data <- dplyr::filter(x, !!rlang::sym(col) == val)
        results <- results %>% dplyr::bind_rows(
          bind_smd_results(
            sub_data, group, weight, variable,
            col, val
          )
        )
      }
    }
  }

  return(results)
}


asmdCategorical <- function(x, variable, group, weight = NULL, results) {
  if (is.null(weight)) {
    x <- x %>% dplyr::mutate(weight = 1)
  } else {
    x <- x %>% dplyr::mutate("weight" = dplyr::all_of(weight))
  }

  columns_to_process <- names(x)[sapply(names(x), function(col_name) {
    # Exclude specific columns from the analysis
    if (col_name %in% c(group, weight, "estimate_value", "variable_name")) {
      FALSE
    } else {
      any(sapply(split(x[[col_name]], x[[group]]), function(subgroup) {
        length(unique(subgroup)) > 1
      }))
    }
  })]

  lab <- unique(x[[group]])
  if (length(lab) > 2) {
    stop("Number of labels in group column different from 2.")
  }


  if (length(columns_to_process) == 0) {
    # No additional columns, compute SMD for the group levels directly
    results <- results %>% dplyr::bind_rows(bind_smd_percentage_results(
      x, group, weight,
      variable, NA
    ))
  } else {
    # Calculate SMD for each combination of subgroups within the variable_name group
    for (col in columns_to_process) {
      unique_values <- unique(x[[col]])

      # Process each unique value in the additional columns
      for (val in unique_values) {
        sub_data <- x %>% dplyr::filter(.data[[col]] == val)
        results <- results %>% dplyr::bind_rows(bind_smd_percentage_results(
          sub_data, group, weight,
          variable, col, val
        ))
      }
    }
  }

  return(results)
}

asmdFromPercentage <- function(TT, CC) {
  TT <- TT / 100
  CC <- CC / 100

  # Calculate the pooled standard deviation
  pooled_sd <- sqrt((TT * (1 - TT) + CC * (1 - CC)) / 2)

  # Calculate SMD
  if (pooled_sd == 0) {
    return(NA) # Return NA if pooled_sd is 0 to avoid division by zero
  } else {
    smd <- (TT - CC) / pooled_sd
    return(smd)
  }
}

# Helper function to bind SMD results
bind_smd_results <- function(data, group, weight, variable, col = NA, val = NA) {
  # Ensure there's enough data to compute SMD
  data <- data %>% dplyr::mutate(estimate_value = as.numeric(estimate_value))
  if (nrow(data) > 1) {
    # Extract mean and sd for each group level
    means <- data %>%
      dplyr::filter(.data$estimate_name == "mean") %>%
      dplyr::select(group, "estimate_value") %>%
      tidyr::pivot_wider(names_from = group, values_from = estimate_value)
    sds <- data %>%
      dplyr::filter(.data$estimate_name == "sd") %>%
      dplyr::select(group, "estimate_value") %>%
      tidyr::pivot_wider(names_from = group, values_from = estimate_value)

    # Compute SMD if both means and SDs are available
    if (!is.na(means[[1]]) && !is.na(means[[2]]) && !is.na(sds[[1]]) && !is.na(sds[[2]])) {
      smd <- (means[[1]] - means[[2]]) / sqrt((sds[[1]]^2 + sds[[2]]^2) / 2)
    } else {
      smd <- NA
    }
    # Store the results
    results <- dplyr::bind_rows(dplyr::tibble(
      variable_name = variable,
      sub_variable = col,
      sub_variable_value = val,
      smd = smd,
      smd_type = "continuous"
    ))
  }
  return(results)
}


# Helper function to compute and bind SMD results
bind_smd_percentage_results <- function(data, group, weight, variable,
                                        sub_variable = NA, sub_variable_value = NA) {
  # Calculate weighted percentages for each group
  lab <- unique(data[[group]])

  percentages <- data %>%
    dplyr::rename("group" = dplyr::all_of(group)) %>%
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value)) %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 0)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(total_weight = sum(weight)) %>%
    dplyr::mutate(weighted_value = estimate_value * weight) %>%
    dplyr::summarise(
      percentage = sum(.data$weighted_value) / .data$total_weight,
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = "group", values_from = "percentage",
      values_fill = list(percentage = 0)
    )


  # Ensure there are exactly two groups to compute SMD

  TT <- percentages[["1"]]
  CC <- percentages[["0"]]
  # Calculate SMD
  if (!is.null(TT) & is.null(CC)) {
    CC <- 0
  } else if (is.null(TT) & !is.null(CC)) {
    TT <- 0
  }

  smd_value <- asmdFromPercentage(TT, CC)

  # Return results as a data frame
  return(
    dplyr::tibble(
      variable_name = variable,
      sub_variable = sub_variable,
      sub_variable_value = sub_variable_value,
      smd = smd_value,
      smd_type = "categorical"
    )
  )
}
