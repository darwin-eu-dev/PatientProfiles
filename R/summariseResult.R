# Copyright 2023 DARWIN EU (C)
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

#' Summarise the characteristics of different individuals
#'
#' @param table Table with different records
#' @param group List of groups to be considered.
#' @param includeOverallGroup TRUE or FALSE. If TRUE, results for an overall
#' group will be reported when a list of groups has been specified.
#' @param strata List of the stratifications within each group to be considered.
#' @param includeOverallStrata TRUE or FALSE. If TRUE, results for an overall
#' strata will be reported when a list of strata has been specified.
#' @param variables List of the different groups of variables, by default they
#' are automatically classified.
#' @param functions List of functions to be applied to each one of the group of
#' variables.
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table that summarises the characteristics of the individual.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(dplyr)
#'
#' cdm <- mockPatientProfiles()
#' x <- cdm$cohort1 %>%
#'   addDemographics(cdm) %>%
#'   collect()
#' result <- summariseResult(x)
#' }
#'
summariseResult <- function(table,
                            group = list(),
                            includeOverallGroup = FALSE,
                            strata = list(),
                            includeOverallStrata = TRUE,
                            variables = list(
                              numericVariables = detectVariables(table, "numeric"),
                              dateVariables = detectVariables(table, "date"),
                              binaryVariables = detectVariables(table, "binary"),
                              categoricalVariables = detectVariables(table, "categorical")
                            ),
                            functions = list(
                              numericVariables = c("median", "q25", "q75"),
                              dateVariables = c("median", "q25", "q75"),
                              binaryVariables = c("count", "percentage"),
                              categoricalVariables = c("count", "percentage")
                            ),
                            minCellCount = 5) {
  # initial checks
  checkTable(table)

  # create the summary for overall
  result <- list()
  if (table %>%
    dplyr::count() %>%
    dplyr::pull() == 0) {
    result <- table %>%
      dplyr::summarise(estimate = as.character(dplyr::n()), .groups = "drop") %>%
      dplyr::mutate(
        variable = "number records", variable_type = as.character(NA),
        estimate_type = "count"
      )
  } else {
    checkStrata(group, table)
    checkStrata(strata, table)
    checkVariablesFunctions(variables, functions, table)
    checkSuppressCellCount(minCellCount)

    # get which are the estimates that are needed
    requiredFunctions <- NULL
    for (nam in names(variables)) {
      requiredFunctions <- requiredFunctions %>%
        dplyr::union_all(
          tidyr::expand_grid(
            variable = variables[[nam]],
            estimate_type = functions[[nam]]
          )
        )
    }
    requiredFunctions <- requiredFunctions %>%
      dplyr::left_join(
        table %>%
          dplyr::ungroup() %>%
          variableTypes() %>%
          dplyr::select(-"type_sum"),
        by = "variable"
      )

    #collect if necessary
    collectFlag <- requiredFunctions %>%
      dplyr::filter(!.data$variable_type %in% c("binary","numeric")) %>%
      nrow() > 0
    if (collectFlag) {
      table <- table %>% dplyr::collect()
    }

    if (isTRUE(includeOverallGroup) || length(group) == 0) {
      result <- table %>%
        summaryValuesStrata(
          strata, requiredFunctions,
          includeOverall = includeOverallStrata
        ) %>%
        dplyr::mutate(
          group_name = "Overall",
          group_level = "Overall"
        ) %>%
        dplyr::select(dplyr::all_of(
          c(
            "group_name", "group_level",
            "strata_name", "strata_level", "variable",
            "variable_level", "variable_type",
            "estimate_type", "estimate"
          )
        )) %>%
        dplyr::arrange(.data$strata_name, .data$strata_level)
    }

    # add results for each group
    for (i in seq_along(group)) {

      table <- table %>%
        dplyr::mutate(
          group_var = !!rlang::parse_expr(uniteStrata(group[[i]]))
        )
      workingGroupLevels <- table %>%
        dplyr::select(dplyr::all_of("group_var")) %>%
        dplyr::distinct() %>%
        dplyr::pull()

      for (j in seq_along(workingGroupLevels)) {
        workingResult <- table %>%
          dplyr::filter(
            .data[["group_var"]] == !!workingGroupLevels[j]
          ) %>%
          summaryValuesStrata(
            strata, requiredFunctions,
            includeOverall = includeOverallStrata
          ) %>%
          dplyr::mutate(
            group_name = !!paste0(group[[i]], collapse = " and "),
            group_level = !!workingGroupLevels[j]
          ) %>%
          dplyr::select(dplyr::all_of(
            c(
              "group_name", "group_level",
              "strata_name", "strata_level", "variable",
              "variable_level", "variable_type",
              "estimate_type", "estimate"
            )
          )) %>%
          dplyr::arrange(.data$strata_name, .data$strata_level)

        result <- dplyr::bind_rows(result, workingResult)
      }
    }

    # obscure counts
    result <- suppressCounts(result, minCellCount = minCellCount)
  }

  return(result)
}

#' @noRd
getNumericValues <- function(x, variablesNumeric) {



  functions <- variablesNumeric %>%
    dplyr::pull("estimate_type") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    functionName <- functions[k]


    variablesFunction <- variablesNumeric %>%
      dplyr::filter(.data$estimate_type == functionName) %>%
      dplyr::pull("variable")
    result <- result %>%
      dplyr::union_all(
        x %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(variablesFunction),
            .fns = getFunctions(functionName),
            .names = "{.col}_{.fn}"
          )) %>%
          tidyr::pivot_longer(
            dplyr::all_of(variablesFunction %>% paste(functionName, sep="_")),
            names_to = "variable",
            values_to = "estimate",
            values_transform = list(estimate = as.character)
          ) %>%
          dplyr::mutate(
            estimate_type = as.character(functionName), variable_type = "numeric"
          ) %>%
          dplyr::select(
            "strata_level", "variable", "variable_type", "estimate_type",
            "estimate"
          ) %>% dplyr::collect() %>%
          dplyr::mutate(variable = stringr::str_replace(.data$variable,  "_[^_]+$", ""))

      )
  }
  return(result)
}

#' @noRd
getDateValues <- function(x, variablesDate) {

  x <- x %>% dplyr::collect()
  functions <- variablesDate %>%
    dplyr::pull("estimate_type") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {

    variablesFunction <- variablesDate %>%
      dplyr::filter(.data$estimate_type == .env$functions[k]) %>%
      dplyr::pull("variable")
    resultK <- x %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(variablesFunction), as.numeric)) %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(variablesFunction),
        .fns = getFunctions(functions[k]),
        .names = "{.col}"
      ))
    if (availableFunctions("date") %>%
      dplyr::filter(.data$format_key == functions[k]) %>%
      dplyr::pull("result") == "date") {
      resultK <- resultK %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
        ))
    }
    resultK <- resultK %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction),
        names_to = "variable",
        values_to = "estimate"
      ) %>%
      dplyr::mutate(
        estimate_type = .env$functions[k], variable_type = "date"
      )
    result <- dplyr::union_all(result, resultK)
  }

  result <- result %>%
    dplyr::select(
      "strata_level", "variable", "variable_type", "estimate_type",
      "estimate"
    )
  return(result)
}

#' @noRd
getBinaryValues <- function(x, variablesBinary) {
  result <- NULL
  variablesFunction <- variablesBinary %>%
    dplyr::filter(.data$estimate_type %in% c("count", "percentage")) %>%
    dplyr::pull("variable") %>%
    unique()
  if (length(variablesFunction) > 0) {
    result <- result %>%
      dplyr::union_all(
        x %>%
          dplyr::mutate(denominator = 1) %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(c(variablesFunction, "denominator")),
            .fns = list("sum" = function(x) {
              sum(x)
            }),
            .names = "{.col}"
          )) %>%
          tidyr::pivot_longer(
            dplyr::all_of(variablesFunction),
            names_to = "variable",
            values_to = "count"
          ) %>%
          dplyr::mutate("percentage" = 100 * .data$count / .data$denominator) %>%
          dplyr::select(-"denominator") %>%
          tidyr::pivot_longer(c("count", "percentage"),
                              names_to = "estimate_type",
                              values_to = "estimate"
          ) %>%
          dplyr::collect() %>%
          dplyr::inner_join(
            variablesBinary %>%
              dplyr::select("variable", "variable_type", "estimate_type"),
            by = c("variable", "estimate_type")
          ) %>%
          dplyr::select(
            "strata_level", "variable", "variable_type", "estimate_type",
            "estimate"
          )
      ) %>%
      dplyr::mutate(estimate = as.character(.data$estimate))
  }
  variablesBinary <- variablesBinary %>%
    dplyr::filter(!(.data$estimate_type %in% c("count", "percentage")))
  if (nrow(variablesBinary) > 0) {
    result <- result %>%
      dplyr::union_all(
        getNumericValues(
          x, variablesBinary
        )
      )
  }
  return(result)
}





#' @noRd
getCategoricalValues <- function(x, variablesCategorical) {
  x <- x %>% dplyr::collect()
  variables <- variablesCategorical %>%
    dplyr::pull("variable") %>%
    unique()
  result <- NULL
  denominator <- x %>%
    dplyr::summarise(denominator = dplyr::n())
  for (v in variables) {
    xx <- x %>%
      dplyr::select("strata_level", "variable_level" = dplyr::all_of(v)) %>%
      dplyr::mutate(variable_level = as.character(.data$variable_level))
    functions <- variablesCategorical %>%
      dplyr::filter(.data$variable == .env$v) %>%
      dplyr::pull("estimate_type") %>%
      unique()
    if (length(functions[functions != "distinct"]) > 0) {
      categories <- xx %>%
        dplyr::ungroup() %>%
        dplyr::select("variable_level") %>%
        dplyr::distinct()
      summaryX <- xx %>%
        dplyr::group_by(.data$variable_level, .add = TRUE) %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::group_split(.data$strata_level) %>%
        lapply(function(x) {
          stra <- unique(x$strata_level)
          x <- x %>%
            dplyr::right_join(categories, by = "variable_level") %>%
            dplyr::mutate(strata_level = .env$stra)
          return(x)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::inner_join(denominator, by = "strata_level") %>%
        dplyr::mutate(count = dplyr::if_else(
          is.na(.data$count), 0, .data$count
        ))
    }
    if ("count" %in% functions | "percentage" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::mutate("percentage" = 100 * .data$count / .data$denominator) %>%
            dplyr::select(-"denominator") %>%
            tidyr::pivot_longer(c("count", "percentage"),
              names_to = "estimate_type",
              values_to = "estimate"
            ) %>%
            dplyr::filter(.data$estimate_type %in% .env$functions) %>%
            dplyr::mutate(
              variable = .env$v, variable_type = "categorical"
            ) %>%
            dplyr::select(
              "strata_level", "variable",
              "variable_level", "variable_type",
              "estimate_type", "estimate"
            )
        )
    }
    if ("distinct" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          xx %>%
            dplyr::summarise(
              estimate = dplyr::n_distinct(.data$variable_level), .groups = "drop"
            ) %>%
            dplyr::mutate(
              variable = .env$v, estimate_type = "distinct",
              variable_type = "categorical"
            )
        )
    }
    functions <- functions[!(functions %in% c("count", "percentage", "distinct"))]
    if (length(functions) > 0) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::summarise(dplyr::across(
              .cols = "count",
              .fns = getFunctions(functions),
              .names = "{.fn}"
            )) %>%
            tidyr::pivot_longer(!"strata_level",
              names_to = "estimate_type",
              values_to = "estimate"
            ) %>%
            dplyr::mutate(
              variable = .env$v, variable_type = "categorical"
            )
        )
    }
  }
  return(result)
}

#' @noRd
summaryValues <- function(x, requiredFunctions) {
  # results
  result <- x %>%
    dplyr::summarise(estimate = as.character(dplyr::n()), .groups = "drop") %>%
    dplyr::mutate(
      variable = "number records", variable_type = as.character(NA),
      estimate_type = "count"
    ) %>%
    dplyr::collect()

  # count subjects
  result <- countSubjects(x) %>%
    dplyr::union_all(result)

  # numeric variables
  variablesNumeric <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "numeric")
  if (nrow(variablesNumeric) > 0) {
    result <- dplyr::union_all(
      result,
      getNumericValues(
        x, variablesNumeric
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  # date variables
  variablesDate <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "date")
  if (nrow(variablesDate) > 0) {
    result <- dplyr::union_all(
      result,
      getDateValues(
        x, variablesDate
      ) %>%
        dplyr::arrange(.data$variable)
    )
  }

  # binary variables
  variablesBinary <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "binary")
  if (nrow(variablesBinary) > 0) {
    result <- dplyr::union_all(
      result,
      getBinaryValues(
        x, variablesBinary
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  result <- result %>%
    dplyr::mutate(variable_level = as.character(NA))

  # categorical variables
  variablesCategorical <- requiredFunctions %>%
    dplyr::filter(.data$variable_type == "categorical")
  if (nrow(variablesCategorical) > 0) {
    result <- dplyr::union_all(
      result,
      getCategoricalValues(
        x, variablesCategorical
      ) %>%
        dplyr::mutate(estimate = as.character(.data$estimate)) %>%
        dplyr::arrange(.data$variable)
    )
  }

  return(result)
}

#' @noRd
countSubjects <- function(x) {
  i <- "person_id" %in% colnames(x)
  j <- "subject_id" %in% colnames(x)
  if (i) {
    if (j) {
      cli::cli_alert_warning(
        "person_id and subject_id present in table, `person_id` used as person identifier"
      )
    }
    personVariable <- "person_id"
  } else if (j) {
    personVariable <- "subject_id"
  }
  if (i | j) {
    result <- x %>%
      dplyr::summarise(
        estimate = as.character(dplyr::n_distinct(.data[[personVariable]])),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = "number subjects", variable_type = as.character(NA),
        estimate_type = "count"
      ) %>%
      dplyr::collect()
    return(result)
  } else {
    return(NULL)
  }
}

#' @noRd
summaryValuesStrata <- function(x,
                                strata,
                                requiredFunctions,
                                includeOverall) {
  result <- list()
  if (isTRUE(includeOverall) || length(strata) == 0) {
    result <- x %>%
      dplyr::mutate(strata_level = "Overall") %>%
      dplyr::group_by(.data$strata_level) %>%
      summaryValues(requiredFunctions) %>%
      dplyr::mutate(strata_name = "Overall")
  }
  for (k in seq_along(strata)) {
    xx <- x %>%
      dplyr::mutate(strata_level = !!rlang::parse_expr(
        uniteStrata(strata[[k]])
      )) %>%
      dplyr::group_by(.data$strata_level)
    result <- result %>%
      dplyr::bind_rows(
        xx %>%
          summaryValues(requiredFunctions) %>%
          dplyr::mutate(strata_name = paste0(strata[[k]], collapse = " and "))
      )
  }
  result <- result %>%
    dplyr::relocate("strata_name")
  return(result)
}

#' Function to suppress counts in summarised objects
#'
#' @param result SummarisedResult object
#' @param minCellCount Minimum count of records to report results.
#'
#' @return Table with suppressed counts
#'
#' @export
suppressCounts <- function(result,
                           minCellCount = 5) {
  checkmate::assertTRUE(all(c(
    "variable", "estimate", "estimate_type", "group_name", "group_level",
    "strata_name", "strata_level"
  ) %in%
    colnames(result)))

  checkSuppressCellCount(minCellCount)

  if (minCellCount > 1) {
    if ("number subjects" %in% result$variable) {
      personCount <- "number subjects"
    } else {
      personCount <- "number records"
    }
    toObscure <- result %>%
      dplyr::filter(.data$variable == .env$personCount) %>%
      dplyr::mutate(estimate = suppressWarnings(as.numeric(.data$estimate))) %>%
      dplyr::filter(.data$estimate > 0 & .data$estimate < .env$minCellCount) %>%
      dplyr::select("group_name", "group_level", "strata_name", "strata_level")
    for (k in seq_len(nrow(toObscure))) {
      ik <- result$group_name == toObscure$group_name[k] &
        result$group_level == toObscure$group_level[k] &
        result$strata_name == toObscure$strata_name[k] &
        result$strata_level == toObscure$strata_level[k]
      is <- result$variable == personCount
      if (sum((ik & is) | is.na(ik & is)) > 0) {
        result$estimate[ik & is] <- paste0("<", minCellCount)
        result$estimate[ik & !is] <- as.character(NA)
      }
    }
    estimate <- suppressWarnings(as.numeric(result$estimate))
    id <- which(
      result$estimate_type == "count" & estimate < minCellCount &
        estimate > 0 & !is.na(estimate)
    )
    x <- result[id, ] %>%
      dplyr::select(-"estimate") %>%
      dplyr::mutate(estimate_type = "percentage")
    result <- result %>%
      dplyr::left_join(
        x %>% dplyr::mutate(obscure_estimate = 1),
        by = colnames(x)
      ) %>%
      dplyr::mutate(estimate = dplyr::if_else(
        !is.na(.data$obscure_estimate), as.character(NA), .data$estimate
      )) %>%
      dplyr::select(-"obscure_estimate")
    result$estimate[id] <- paste0("<", minCellCount)
  }
  return(result)
}

uniteStrata <- function(columns,
                        sepStrataLevel = " and ") {
  pasteStr <- paste0(
    "paste0(",
    paste0(
      "as.character(.data[[\"", columns, "\"]])",
      collapse = paste0(
        ", \"", sepStrataLevel, "\", "
      )
    ),
    ")"
  )
  return(pasteStr)
}
