#' Summarise the characteristics of different individuals
#'
#' @param table Table with different records
#' @param strata List of the stratifications to be considered.
#' @param variables List of the different groups of variables, by default they
#' are automatically classified.
#' @param functions List of functions to be applied to each one of the group of
#' variables.
#' @param suppressCellCount Minimum count of records to report results.
#' @param bigMark Big mark delimiter.
#' @param decimalMark Decimal separator.
#' @param significantDecimals Number of significant decimals reported.
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
#' result <- summariseCharacteristics(x)
#' }
#'
summariseCharacteristics <- function (table,
                                      strata = list(),
                                      variables = list(
                                        numericVariables = detectVariables(table, "numeric"),
                                        dateVariables = detectVariables(table, "date"),
                                        binaryVariables = detectVariables(table, "binary"),
                                        categoricalVariables = detectVariables(table, "categorical")
                                      ),
                                      functions = list(
                                        numericVariables = c("median", "q25", "q75"),
                                        dateVariables = c("median", "q25", "q75"),
                                        binaryVariables = c("count", "%"),
                                        categoricalVariables = c("count", "%")
                                      ),
                                      suppressCellCount = 5,
                                      bigMark = ",",
                                      decimalMark = ".",
                                      significantDecimals = 2) {
  # initial checks
  checkTable(table)
  checkStrata(strata, table)
  checkVariablesFunctions(variables, functions, table)
  checkSuppressCellCount(suppressCellCount)
  checkBigMark(bigMark)
  checkDecimalMark(decimalMark)
  checkSignificantDecimals(significantDecimals)

  # create the summary
  result <- table %>%
    summaryValuesStrata(
      strata, variables, functions, bigMark, decimalMark, significativeDecimals
    )

  # obscure counts
  # result <- supressCounts(result, suppressCellCount)

  return(result)
}

#' @noRd
getNumericValues <- function(x, variablesNumeric, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesNumeric %>%
    dplyr::pull("estimate") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesNumeric %>%
      dplyr::filter(.data$estimate == .env$functions[k]) %>%
      dplyr::pull("variable")
    result <- result %>%
      dplyr::union_all(
        x %>%
          dplyr::summarise(dplyr::across(
            .cols = dplyr::all_of(variablesFunction),
            .fns = getFunctions(functions[k]),
            .names = "{.col}"
          )) %>%
          dplyr::mutate(dplyr::across(
            dplyr::all_of(variablesFunction),
            ~ niceNum(., bigMark, decimalMark, significativeDecimals)
          )) %>%
          tidyr::pivot_longer(
            dplyr::all_of(variablesFunction), names_to = "variable"
          ) %>%
          dplyr::mutate(
            estimate = .env$functions[k], variable_classification = "numeric"
          ) %>%
          dplyr::select(
            "strata_level", "variable", "variable_classification", "estimate",
            "value"
          )
      )
  }
  return(result)
}

#' @noRd
getDateValues <- function(x, variablesDate, groupVariable, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesDate %>%
    dplyr::pull("estimate") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesDate %>%
      dplyr::filter(.data$estimate == .env$functions[k]) %>%
      dplyr::pull("variable")
    result.k <- x %>%
      dplyr::summarise(dplyr::across(
        .cols = dplyr::all_of(variablesFunction),
        .fns = getFunctions(functions[k]),
        .names = "{.col}"
      ))
    if (availableFunctions("date") %>%
        dplyr::filter(.data$fomrat_key == functions[k]) %>%
        dplyr::pull("result") == "date") {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ as.character(as.Date(round(.x), origin = "1970-01-01"))
        ))
    } else {
      result.k <- result.k %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(variablesFunction),
          ~ niceNum(.x, bigMark, decimalMark, significativeDecimals)
        ))
    }
    result.k <- result.k %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction), names_to = "variable"
      ) %>%
      dplyr::mutate(
        estimate = .env$functions[k], variable_classification = "date"
      )
    result <- dplyr::union_all(result, result.k)
  }
  result <- result %>%
    dplyr::select(
      "strata_level", "variable", "variable_classification", "estimate",
      "value"
    )
  return(result)
}

#' @noRd
getBinaryValues <- function(x, variablesBinary, groupVariable, bigMark, decimalMark, significativeDecimals) {
  variablesFunction <- variablesBinary %>%
    dplyr::pull("variable") %>%
    unique()
  result <- x %>%
    dplyr::mutate(denominator = 1) %>%
    dplyr::summarise(dplyr::across(
      .cols = dplyr::all_of(c(variablesFunction, "denominator")),
      .fns = list("sum" = function(x) {sum(x)}),
      .names = "{.col}"
    )) %>%
    tidyr::pivot_longer(
      dplyr::all_of(variablesFunction),
      names_to = "variable",
      values_to = "count"
    ) %>%
    dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
    dplyr::mutate(
      "%" = base::paste0(
        niceNum(.data[["%"]], bigMark, decimalMark, significativeDecimals),
        "%"
      ),
      count = niceNum(.data$count, bigMark, decimalMark, significativeDecimals)
    ) %>%
    dplyr::select(-"denominator") %>%
    tidyr::pivot_longer(c("count", "%"), names_to = "estimate") %>%
    dplyr::inner_join(
      variablesBinary %>%
        dplyr::select("variable", "variable_classification", "estimate"),
      by = c("variable", "estimate")
    ) %>%
    dplyr::select(
      "strata_level", "variable", "variable_classification", "estimate",
      "value"
    )
  return(result)
}

#' @noRd
getCategoricalValues <- function(x, variablesCategorical, groupVariable, bigMark, decimalMark, significativeDecimals) {
  variables <- variablesCategorical %>%
    dplyr::pull("variable") %>%
    unique()
  result <- NULL
  denominator <- x %>%
    dplyr::summarise(denominator = dplyr::n())
  for (v in variables) {
    xx <- x %>%
      dplyr::select("strata_level", "category" = dplyr::all_of(v)) %>%
      tidyr::separate_rows("category", sep = "&&", convert = TRUE)
    functions <- variablesCategorical %>%
      dplyr::filter(.data$variable == .env$v) %>%
      dplyr::pull("estimate") %>%
      unique()
    if (length(functions[functions != "distinct"]) >  0) {
      categories <- xx %>%
        dplyr::ungroup() %>%
        dplyr::select("category") %>%
        dplyr::distinct()
      summaryX <- xx %>%
        dplyr::group_by(.data$category, add = TRUE) %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::right_join(categories, by = "category") %>%
        dplyr::right_join(denominator, by = "strata_level") %>%
        dplyr::mutate(count = dplyr::if_else(
          is.na(.data$count), 0, .data$count
        ))
    }
    if ("count" %in% functions | "%" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
            dplyr::mutate(
              "%" = base::paste0(niceNum(
                .data[["%"]], bigMark, decimalMark, significativeDecimals
              ), "%"),
              count = niceNum(
                .data$count, bigMark, decimalMark, significativeDecimals
              )
            ) %>%
            dplyr::select(-"denominator") %>%
            tidyr::pivot_longer(c("count", "%"), names_to = "estimate") %>%
            dplyr::filter(.data$estimate %in% .env$functions) %>%
            dplyr::mutate(
              variable = .env$v, variable_classification = "categorical",
              estimate = paste0(.data$category, ": ", .data$estimate)
            ) %>%
            dplyr::select(
              "strata_level", "variable", "variable_classification",
              "estimate", "value"
            )
        )
    }
    if ("distinct" %in% functions) {
      result <- result %>%
        dplyr::union_all(
          xx %>%
            dplyr::summarise(
              value = dplyr::n_distinct(.data$category), .groups = "drop"
            ) %>%
            dplyr::mutate(
              value = format(round(.data$value), big.mark = bigMark),
              variable = .env$v, estimate = "distinct",
              variable_classification = "categorical"
            )
        )
    }
    functions <- functions[!(functions %in% c("count", "%", "distinct"))]
    if (length(functions) > 0) {
      result <- result %>%
        dplyr::union_all(
          summaryX %>%
            dplyr::summarise(dplyr::across(
              .cols = "count",
              .fns = getFunctions(functions),
              .names = "{.fn}"
            )) %>%
            tidyr::pivot_longer(!"strata_level", names_to = "estimate") %>%
            dplyr::mutate(
              variable = .env$v, variable_classification = "categorical",
              value = niceNum(
                .data$value, bigMark, decimalMark, significativeDecimals
              )
            )
        )
    }
  }
  return(result)
}

#' @noRd
summaryValues <- function(x, variables, functions, bigMark, decimalMark, significativeDecimals) {
  # get which are the estimates that are needed
  requiredFunctions <- NULL
  for (nam in names(variables)) {
    requiredFunctions <- requiredFunctions %>%
      dplyr::union_all(
        tidyr::expand_grid(
          variable = variables[[nam]],
          estimate = functions[[nam]]
        )
      )
  }
  requiredFunctions <- requiredFunctions %>%
    dplyr::left_join(
      variableTypes(x) %>% dplyr::select(-"variable_type"), by = "variable"
    )

  # results
  result <- x %>%
    dplyr::summarise(value = as.character(dplyr::n()), .groups = "drop") %>%
    dplyr::mutate(
      variable = "number records", variable_classification = as.character(NA),
      estimate = "count"
    )

  # count subjects
  result <- countSubjects(x) %>%
    dplyr::union_all(result)

  # numeric variables
  variablesNumeric <- requiredFunctions %>%
    dplyr::filter(.data$variable_classification == "numeric")
  if (nrow(variablesNumeric) > 0) {
    result <- dplyr::union_all(
      result,
      getNumericValues(
        x, variablesNumeric, bigMark, decimalMark, significativeDecimals
      )
    )
  }

  # date variables
  variablesDate <- requiredFunctions %>%
    dplyr::filter(.data$variable_classification == "date")
  if (nrow(variablesDate) > 0) {
    result <- dplyr::union_all(
      result,
      getDateValues(
        x, variablesDate, bigMark, decimalMark, significativeDecimals
      )
    )
  }

  # binary variables
  variablesBinary <- requiredFunctions %>%
    dplyr::filter(.data$variable_classification == "binary")
  if (nrow(variablesBinary) > 0) {
    result <- dplyr::union_all(
      result,
      getBinaryValues(
        x, variablesBinary, bigMark, decimalMark, significativeDecimals
      )
    )
  }

  # categorical variables
  variablesCategorical <- requiredFunctions %>%
    dplyr::filter(.data$variable_classification == "categorical")
  if (nrow(variablesCategorical) > 0) {
    result <- dplyr::union_all(
      result,
      getCategoricalValues(
        x, variablesCategorical, bigMark, decimalMark, significativeDecimals
      )
    )
  }
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
        value = as.character(dplyr::n_distinct(.data[[personVariable]])),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = "number subjects", variable_classification = as.character(NA),
        estimate = "count"
      )
    return(result)
  } else {
    return(NULL)
  }
}

#' @noRd
summaryValuesStrata <- function(x, strata, variables, functions, bigMark, decimalMark, significativeDecimals) {
  result <- x %>%
    dplyr::mutate(strata_level = as.character(NA)) %>%
    dplyr::group_by(.data$strata_level) %>%
    summaryValues(
      variables, functions, bigMark, decimalMark, significativeDecimals
    ) %>%
    dplyr::mutate(strata_name = "overall")
  for (strat in names(strata)) {
    xx <- x %>%
      tidyr::unite(
        "strata_level", dplyr::all_of(strata[[strat]]), remove = FALSE,
        sep = "&&"
      ) %>%
      tidyr::separate_rows(.data$strata_level, sep = "&&", convert = TRUE) %>%
      dplyr::group_by(.data$strata_level)
    result <- result %>%
      dplyr::union_all(
        xx %>%
          summaryValues(
            variables, functions, bigMark, decimalMark, significativeDecimals
          ) %>%
          dplyr::mutate(strata_name = .env$strat)
      )
  }
  result <- result %>%
    dplyr::relocate("strata_name")
  return(result)
}

#' @noRd
niceNum <- function(x, bigMark, decimalMark, significativeDecimals) {
  if (all(x %% 1 == 0)) {
    significativeDecimals <- 0
  }
  base::format(
    round(x, significativeDecimals),
    big.mark = bigMark,
    decimal.mark = decimalMark,
    nsmall = significativeDecimals
  )
}
