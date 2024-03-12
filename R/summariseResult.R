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

#' Summarise variables using a set of estimate functions. The output will be
#' a formatted summaried_result object.
#'
#' @param table Table with different records
#' @param group List of groups to be considered.
#' @param includeOverallGroup TRUE or FALSE. If TRUE, results for an overall
#' group will be reported when a list of groups has been specified.
#' @param strata List of the stratifications within each group to be considered.
#' @param includeOverallStrata TRUE or FALSE. If TRUE, results for an overall
#' strata will be reported when a list of strata has been specified.
#' @param variables Variables to summarise, it can be a list to point to different
#' set of estimate names.
#' @param estimates Estimates to obtain, it can be a list to point to different
#' set of variables.
#' @param functions deprecated.
#' @param counts Whether to compute number of records and number of subjects.
#' @param verbose Whether to print progress.
#'
#' @return A summarised_result object with the summarised data of interest.
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
#'   addDemographics() %>%
#'   collect()
#' result <- summariseResult(x)
#' }
#'
summariseResult <- function(table,
                            group = list(),
                            includeOverallGroup = FALSE,
                            strata = list(),
                            includeOverallStrata = TRUE,
                            variables = NULL,
                            functions = lifecycle::deprecated(),
                            estimates = c("min", "q25", "median", "q75", "max", "count", "percentage"),
                            counts = TRUE,
                            verbose = TRUE) {
  if (!lifecycle::is_present(functions)) {
    lifecycle::deprecate_warn(
      when = "0.7.0",
      what = "summariseResult(functions)",
      with = "summariseResult(estimates)"
    )
    if (missing(estimates)) {
      estimates <- functions
    }
    rm(functions)
  }

  # initial checks
  checkTable(table)

  if (is.null(variables)) {
    variables <- colnames(table)
    variables <- variables[!grepl("_id", variables)]
  }

  if (inherits(table, "cdm_table")) {
    cdm_name <- omopgenerics::cdmName(omopgenerics::cdmReference(table))
  } else {
    cdm_name <- "unknown"
  }

  # create the summary for overall
  if (table %>%
    dplyr::count() %>%
    dplyr::pull() == 0) {
    if (counts) {
      result <- dplyr::tibble(
        "group_name" = "overall", "group_level" = "overall",
        "strata_name" = "overall", "strata_level" = "overall",
        "variable_name" = c("number_records", "number_subjects"),
        "variable_level" = NA_character_, "estimate_name" = "count",
        "estimate_type" = "integer", "estimate_value" = "0"
      )
    } else {
      result <- omopgenerics::emptySummarisedResult()
      return(result)
    }
  } else {
    if (!is.list(variables)) {
      variables <- list(variables)
    }
    if (!is.list(estimates)) {
      estimates <- list(estimates)
    }
    checkStrata(group, table)
    checkStrata(strata, table)
    functions <- checkVariablesFunctions(variables, estimates, table)

    if (nrow(functions) == 0) {
      cli::cli_alert_warning(
        "No estimate can be computed with the current arguments, check `availableEstimates()` for the estimates that this function supports"
      )
      return(omopgenerics::emptySummarisedResult())
    } else if (verbose) {
      mes <- c("i" = "The following estimates will be computed:")
      variables <- functions$variable_name |> unique()
      for (vark in variables) {
        mes <- c(mes, "*" = paste0(
          vark, ": ", paste0(functions$estimate_name[functions$variable_name == vark], collapse = ", ")
        ))
      }
      cli::cli_inform(message = mes)
    }

    # only required variables
    table <- table |>
      dplyr::select(dplyr::any_of(unique(c(
        unlist(strata), unlist(group), functions$variable_name, "person_id",
        "subject_id"
      ))))

    # collect if necessary
    collectFlag <- functions %>%
      dplyr::filter(grepl("q", .data$estimate_name)) %>%
      nrow() > 0
    if (collectFlag) {
      table <- table %>% dplyr::collect()
    }

    # correct dates and logicals
    dates <- functions |>
      dplyr::filter(.data$variable_type %in% c("date", "logical")) |>
      dplyr::distinct(.data$variable_name) |>
      dplyr::pull()
    table <- table |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(dates),
        .fns = as.integer
      ))

    # correct strata and group
    group <- correctStrata(group, includeOverallGroup)
    strata <- correctStrata(strata, includeOverallStrata)

    resultk <- 1
    result <- list()
    for (groupk in group) {
      for (stratak in strata) {
        result[[resultk]] <- summariseInternal(
          table, groupk, stratak, functions, counts
        )
        resultk <- resultk + 1
      }
    }
    result <- result |> dplyr::bind_rows()
  }

  result <- result |>
    dplyr::rename(
      "variable_name" = "variable", "estimate_name" = "estimate_type",
      "estimate_value" = "estimate"
    ) |>
    dplyr::left_join(
      formats |>
        dplyr::select(
          "estimate_name" = "format_key",
          "variable_type",
          "estimate_type" = "result"
        ),
      relationship = "many-to-many",
      by = c("estimate_name", "variable_type")
    ) |>
    dplyr::mutate(
      "estimate_type" = dplyr::if_else(
        .data$estimate_name %in% c("count_missing", "percentage_missing"),
        "numeric",
        .data$estimate_type
      ),
      "cdm_name" = .env$cdm_name,
      "result_type" = "summarise_table",
      "package_name" = "PatientProfiles",
      "package_version" = as.character(utils::packageVersion("PatientProfiles")),
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    dplyr::select(dplyr::all_of(c(
      "cdm_name", "result_type", "package_name", "package_version",
      "group_name", "group_level", "strata_name", "strata_level",
      "variable_name", "variable_level", "estimate_name", "estimate_type",
      "estimate_value", "additional_name", "additional_level"
    ))) |>
    omopgenerics::newSummarisedResult()

  return(result)
}

summariseInternal <- function(table, groupk, stratak, functions, counts) {
  result <- list()

  # group by relevant variables
  table <- table |> dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(
    groupk, stratak
  )))))

  # count subjects and records
  if (counts) {
    result$counts <- countSubjects(table)
  }

  # summariseNumeric
  result$numeric <- summariseNumeric(table, functions)

  # summariseBinary

  # summariseCategories

  # summariseMissings

  # fomat group strata
  result <- result |>
    dplyr::bind_rows() |>
    visOmopResults::uniteGroup(cols = groupk) |>
    visOmopResults::uniteStrata(cols = stratak)

  return(result)
}

summariseNumeric <- function(table, functions) {
  funs <- functions |>
    dplyr::filter(
      .data$variable_type %in% c("date", "numeric", "integer") &
        !grepl("count|percentage", .data$estimate_name)
    )
  res <- list()
  uniqueEstimates <- funs$estimate_name |> unique()
  uniqueVariables <- funs$variable_name |> unique()
  if (length(uniqueEstimates) <= length(uniqueVariables)) {
    for (est in uniqueEstimates) {
      varEst <- funs |>
        dplyr::filter(.data$estimate_name == .env$est) |>
        dplyr::pull("variable_name")
      res[[est]] <- table |>
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::all_of(varEst),
            .fns = getFunctions(est),
            .names = "{.col}"
          ),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(varEst), .fns = as.character
        )) |>
        tidyr::pivot_longer(
          cols = varEst,
          names_to = "variable_name",
          values_to = "estimate_value"
        )
    }
    res <- res |> dplyr::bind_rows()
  } else {

  }
  for (k in seq_along(funGroups$fun)) {
    funk <- getFunctions(funGroups$fun[[k]])
    res[[k]] <- table |>
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::all_of(funGroups$cols[[k]]),
          .fns = funk
        ),
        .groups = "drop"
      ) |>
      dplyr::collect()
  }

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
    } else {
      resultK <- resultK %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction), as.character
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
              sum(x, na.rm = TRUE)
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
            dplyr::group_by(.data$strata_level) %>%
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
              variable = .env$v, variable_type = "categorical",
              variable_level = as.character(NA)
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
      variable = "number records", variable_type = "categorical",
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
        dplyr::mutate(estimate = as.character(.data$estimate))
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
      )
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
        dplyr::mutate(estimate = as.character(.data$estimate))
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
      getCategoricalValues(x, variablesCategorical) %>%
        dplyr::mutate(estimate = as.character(.data$estimate))
    )
  }

  # arrange data
  result <- arrangeSummary(result, colnames(x), requiredFunctions)

  # add percentage to missing values
  result <- correctMissing(result)

  return(result)
}

#' @noRd
countSubjects <- function(x) {
  i <- "person_id" %in% colnames(x)
  j <- "subject_id" %in% colnames(x)
  if (i) {
    if (j) {
      cli::cli_warn(
        "person_id and subject_id present in table, `person_id` used as person
        identifier"
      )
    }
    personVariable <- "person_id"
  } else if (j) {
    personVariable <- "subject_id"
  }
  if (i | j) {
    result <- x %>%
      dplyr::summarise(
        "number_records" = dplyr::n(),
        "number_subjects" = dplyr::n_distinct(.data[[personVariable]]),
        .groups = "drop"
      ) %>%
      dplyr::collect() |>
      tidyr::pivot_longer(
        cols = c("number_records", "number_subjects"),
        names_to = "variable_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "estimate_type" = "integer",
        "estimate_name" = "count",
        "variable_level" = NA_character_,
        "estimate_value" = as.character(.data$estimate_value)
      )
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
      dplyr::mutate(strata_level = "overall") %>%
      dplyr::group_by(.data$strata_level) %>%
      summaryValues(requiredFunctions) %>%
      dplyr::mutate(strata_name = "overall")
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

correctMissing <- function(result) {
  if ("missing" %in% result$estimate_type) {
    result <- result %>%
      dplyr::mutate(order_id = dplyr::row_number())
    x <- result %>%
      dplyr::filter(.data$estimate_type == "missing")
    xCount <- x %>%
      dplyr::mutate(estimate_type = "count_missing")
    xPercentage <- x %>%
      dplyr::left_join(
        result %>%
          dplyr::filter(.data$variable == "number records") %>%
          dplyr::select("strata_level", "denominator" = "estimate"),
        by = "strata_level"
      ) %>%
      dplyr::mutate(
        estimate = 100*as.numeric(.data$estimate)/as.numeric(.data$denominator),
        estimate_type = "percentage_missing",
        order_id = .data$order_id + 0.5
      ) %>%
      dplyr::select(-"denominator")
    result <- result %>%
      dplyr::filter(.data$estimate_type != "missing") %>%
      dplyr::union_all(
        xCount %>%
          dplyr::mutate(estimate = as.character(.data$estimate))
      ) %>%
      dplyr::union_all(
        xPercentage %>%
          dplyr::mutate(estimate = as.character(.data$estimate))
      ) %>%
      dplyr::arrange(.data$order_id) %>%
      dplyr::select(-"order_id")
  }
  return(result)
}

arrangeStrata <- function(result, strata) {
  namesStrata <- lapply(strata, function(x) {
    paste0(x, collapse = " and ")
  }) %>%
    unlist()
  namesStrata <- c("overall", namesStrata)
  result <- result %>%
    dplyr::inner_join(
      dplyr::tibble(strata_name = namesStrata) %>%
        dplyr::mutate(id = dplyr::row_number()),
      by = "strata_name"
    ) %>%
    dplyr::arrange(.data$id, .data$strata_level) %>%
    dplyr::select(-"id")
  return(result)
}

arrangeSummary <- function(result, columnNames, functions) {
  x <- unique(result$variable)
  orderVariables <- c(x[!(x %in% columnNames)], columnNames[columnNames %in% x])
  result <- result %>%
    dplyr::left_join(
      dplyr::tibble(variable = orderVariables) %>%
        dplyr::mutate(id1 = dplyr::row_number()),
      by = "variable"
    ) %>%
    dplyr::mutate(id2 = .data$variable_level) %>%
    dplyr::left_join(
      functions %>%
        dplyr::select("estimate_type") %>%
        dplyr::distinct() %>%
        dplyr::mutate(id3 = dplyr::row_number()),
      by = "estimate_type"
    ) %>%
    dplyr::arrange(.data$id1, .data$id2, .data$id3) %>%
    dplyr::select(-c("id1", "id2", "id3"))
  return(result)
}
