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
}

#' @noRd
getNumericValues <- function(x, variablesNumeric, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesNumeric %>%
    dplyr::pull("estimate") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesNumeric %>%
      dplyr::filter(.data$fun == .env$functions[k]) %>%
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
    dplyr::pull("fun") %>%
    unique()
  result <- NULL
  for (k in seq_along(functions)) {
    variablesFunction <- variablesDate %>%
      dplyr::filter(.data$fun == .env$functions[k]) %>%
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
      tidyr::pivot_longer(dplyr::all_of(variablesFunction), names_to = "variable") %>%
      dplyr::mutate(
        category = as.character(NA), fun = .env$functions[k],
        variable_classification = "date"
      )
    result <- dplyr::union_all(result, result.k)
  }
  return(result)
}

#' @noRd
getBinaryValues <- function(x, variablesBinary, groupVariable, bigMark, decimalMark, significativeDecimals) {
  variablesFunction <- variablesBinary %>%
    dplyr::pull("variable") %>%
    unique()
  if (!is.null(groupVariable)) {
    result <- x %>%
      dplyr::group_by(.data[[groupVariable]])
  } else {
    result <- x
  }
  result <- result %>%
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
    tidyr::pivot_longer(c("count", "%"), names_to = "fun") %>%
    dplyr::inner_join(
      variablesBinary %>%
        dplyr::select("variable", "variable_classification", "fun"),
      by = c("variable", "fun")
    ) %>%
    dplyr::mutate(category = as.character(NA))
  result <- renameGroupping(result, groupVariable)
  return(result)
}

#' @noRd
getCategoricalValues <- function(x, variablesCategorical, groupVariable, bigMark, decimalMark, significativeDecimals) {
  functions <- variablesCategorical %>%
    dplyr::pull("fun") %>%
    unique()
  result <- dplyr::tibble(
    variable = character(),
    variable_classification = character(),
    fun = character(),
    category = character(),
    value = character(),
    groupping = character()
  )
  if ("count" %in% functions || "%" %in% functions) {
    variablesFunction <- variablesCategorical %>%
      dplyr::filter(.data$fun %in% c("count", "%")) %>%
      dplyr::pull("variable") %>%
      unique()
    for (k in seq_along(variablesFunction)) {
      categories <- unique(x[[variablesFunction[k]]])
      if (is.null(groupVariable)) {
        categories <- dplyr::tibble(category = categories)
        toJoin <- "category"
        result.k <- x %>%
          dplyr::rename("category" = dplyr::all_of(variablesFunction[k])) %>%
          dplyr::group_by(.data$category)
      } else {
        groups <- x %>%
          dplyr::pull(dplyr::all_of(groupVariable)) %>%
          unique()
        categories <- tidyr::expand_grid(
          !!groupVariable := groups,
          category = categories
        )
        toJoin <- c(groupVariable, "category")
        result.k <- x %>%
          dplyr::rename("category" = dplyr::all_of(variablesFunction[k])) %>%
          dplyr::group_by(.data[[groupVariable]], .data$category)
      }
      result.k <- result.k %>%
        dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop") %>%
        dplyr::right_join(categories, by = dplyr::all_of(toJoin)) %>%
        dplyr::mutate(count = dplyr::if_else(
          is.na(.data$count), 0, .data$count
        ))
      if (!is.null(groupVariable)) {
        result.k <- dplyr::group_by(result.k, .data[[groupVariable]])
      }
      result.k <- result.k %>%
        dplyr::mutate(denominator = as.numeric(sum(.data$count))) %>%
        dplyr::mutate("%" = 100 * .data$count / .data$denominator) %>%
        dplyr::mutate(
          "%" = base::paste0(
            niceNum(
              .data[["%"]], bigMark, decimalMark, significativeDecimals
            ),
            "%"
          ),
          count = niceNum(
            .data$count, bigMark, decimalMark, significativeDecimals
          )
        ) %>%
        dplyr::select(-"denominator") %>%
        tidyr::pivot_longer(c("count", "%"), names_to = "fun") %>%
        dplyr::inner_join(
          variablesCategorical %>%
            dplyr::filter(.data$variable == .env$variablesFunction[k]) %>%
            dplyr::filter(.data$fun %in% c("count", "%")) %>%
            dplyr::select("variable", "variable_classification", "fun"),
          by = "fun"
        )
      result <- dplyr::union_all(result, renameGroupping(result.k, groupVariable))
    }
  }
  if ("distinct" %in% functions) {
    variablesFunction <- variablesCategorical %>%
      dplyr::filter(.data$fun == "distinct") %>%
      dplyr::pull("variable")
    if (!is.null(groupVariable)) {
      result.k <- x %>%
        dplyr::group_by(.data[[groupVariable]])
    } else {
      result.k <- x
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        dplyr::all_of(variablesFunction),
        list("distinct" = function(x){dplyr::n_distinct(x)}),
        .names = "{.col}"
      )) %>%
      tidyr::pivot_longer(
        dplyr::all_of(variablesFunction), names_to = "variable"
      ) %>%
      dplyr::mutate(value = format(
        round(.data$value), big.mark = bigMark
      )) %>%
      dplyr::mutate(
        category = as.character(NA), fun = "distinct",
        variable_classification = "categorical"
      )
    result <- dplyr::union_all(result, renameGroupping(result.k, groupVariable))
  }
  variablesCategorical <- variablesCategorical %>%
    dplyr::filter(!(.data$fun %in% c("count", "%", "distinct")))
  variablesFunction <- unique(variablesCategorical$variable)
  for (k in seq_along(variablesFunction)) {
    functions <- variablesCategorical %>%
      dplyr::filter(.data$variable == .env$variablesFunction[k]) %>%
      dplyr::pull("fun")
    categories <- x %>%
      dplyr::pull(dplyr::all_of(variablesFunction[k])) %>%
      unique()
    if (is.null(groupVariable)) {
      categories <- dplyr::tibble(category = categories)
      toJoin <- "category"
      result.k <- x %>%
        dplyr::select(
          dplyr::all_of(groupVariable),
          "category" = dplyr::all_of(variablesFunction[k])
        ) %>%
        dplyr::group_by(.data$category)
    } else {
      groups <- x %>%
        dplyr::pull(dplyr::all_of(groupVariable)) %>%
        unique()
      categories <- tidyr::expand_grid(
        !!groupVariable := groups,
        category = categories
      )
      toJoin <- c(groupVariable, "category")
      result.k <- x %>%
        dplyr::select(
          dplyr::all_of(groupVariable),
          "category" = dplyr::all_of(variablesFunction[k])
        ) %>%
        dplyr::group_by(.data[[groupVariable]], .data$category)
    }
    result.k <- result.k %>%
      dplyr::summarise(count_per_category = dplyr::n(), .groups = "drop") %>%
      dplyr::right_join(categories, by = dplyr::all_of(toJoin)) %>%
      dplyr::mutate(count_per_category = dplyr::if_else(
        is.na(.data$count_per_category), 0, .data$count_per_category
      ))
    if (!is.null(groupVariable)) {
      result.k <- dplyr::group_by(result.k, .data[[groupVariable]])
    }
    result.k <- result.k %>%
      dplyr::summarise(dplyr::across(
        .cols = "count_per_category",
        .fns = getFunctions(functions),
        .names = "{.fn}"
      )) %>%
      tidyr::pivot_longer(dplyr::all_of(functions), names_to = "fun") %>%
      dplyr::mutate(
        variable = .env$variablesFunction[k],
        variable_classification = "categorical"
      ) %>%
      dplyr::mutate(
        value = niceNum(
          .data$value, bigMark, decimalMark, significativeDecimals
        ),
        category = as.character(NA)
      )
    result <- dplyr::union_all(result, renameGroupping(result.k, groupVariable))
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

  # start empty results
  result <- dplyr::tibble(
    strata_level = character(),
    variable = character(),
    variable_classification = character(),
    estimate = character(),
    value = character()
  )

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

