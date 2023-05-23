#' Classify the variables between 5 types: "numeric", "categorical", "binary",
#' "date", or NA.
#'
#' @param table Tibble
#'
#' @return Tibble with the variables type and classification
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' variableTypes(x)
#' }
#'
#' @export
#'
variableTypes <- function(table) {
  checkTable(table)
  x <- dplyr::tibble(
    variable = colnames(table),
    variable_type = lapply(table, pillar::type_sum) %>% unlist()
  ) %>%
    dplyr::mutate(variable_classification = assertClassification(.data$variable_type, .env$table))
  return(x)
}

#' @noRd
assertClassification <- function(x, tib) {
  lapply(seq_along(x), function(i) {
    if (x[i] == "lgl") {
      return("binary")
    } else if (x[i] %in% c("chr", "fct", "ord")) {
        return("categorical")
    } else if (x[i] %in% c("date", "dttm")) {
      return("date")
    } else if (x[i] == "drtn") {
      return("numeric")
    } else if (x[i] %in% c("int", "dbl", "int64")) {
      lab <- unique(tib[[i]])
      if (length(lab) <= 2 && all(lab %in% c(0, 1))) {
        return("binary")
      } else {
        return("numeric")
      }
    } else {
      return(as.character(NA))
    }
  }) %>%
    unlist()
}

#' Show the available functions for the 4 classifications of data that are
#' supported (numeric, date, binary and categorical)
#'
#' @param variableClassification A choice between: "numeric", "date", "binary"
#' or "categorical".
#'
#' @return A tibble with the available functions for a certain variable
#' classification (or all if NULL)
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' availableFunctions()
#' availableFunctions("numeric")
#' availableFunctions("date")
#' availableFunctions("binary")
#' availableFunctions("categorical")
#' }
#'
#' @export
#'
availableFunctions <- function(variableClassification = NULL) {
  if (is.null(variableClassification)) {
    return(formats)
  } else{
    checkVariableClassification(variableClassification)
    x <- formats %>%
      dplyr::filter(.data$type == .env$variableClassification) %>%
      dplyr::select(-"type")
    if (sum(is.na(x$info)) == nrow(x)) {
      x <- x %>% dplyr::select(-"info")
    }
    if (sum(is.na(x$are_NA_considered)) == nrow(x)) {
      x <- x %>% dplyr::select(-"are_NA_considered")
    }
    if (sum(is.na(x$warnings)) == nrow(x)) {
      x <- x %>% dplyr::select(-"warnings")
    }
    return(x)
  }
}

#' Detect automatically variables with a certain classification
#'
#' @param table Tibble
#' @param variableClassification Classification of interest, choice between
#' "numeric", "date", "binary" and "categorical"
#' @param exclude Variables to exclude
#'
#' @return Variables in x with the desired classification
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' detectVariables(x, "numeric")
#' }
#'
#' @export
#'
detectVariables <- function(table,
                            variableClassification,
                            exclude = c(
                              "person_id", "subject_id", "cohort_definition_id",
                              "cohort_name", "strata_name", "strata_level"
                            )) {
  # initial checks
  checkTable(table)
  checkVariableClassification(variableClassification)
  checkExclude(exclude)

  # get variable types
  variables <- variableTypes(table) %>%
    dplyr::filter(
      .data$variable_classification == .env$variableClassification
    ) %>%
    dplyr::pull("variable")

  # eliminate excluded variables
  variables <- variables[!(variables %in% exclude)]

  return(variables)
}
