#' Available functions and formats for numeric variables
#'
#' @return Tibble with the available numeric format keys
#'
#' @examples
#' library(PatientProfiles)
#' numericFormats()
#'
#' @export
#'
numericFormats <- function() {
  return(availableFormat("numeric"))
}

#' Available functions and formats for date variables
#'
#' @return Tibble with the available date format keys
#'
#' @examples
#' library(PatientProfiles)
#' dateFormats()
#'
#' @export
#'
dateFormats <- function() {
  return(availableFormat("date"))
}

#' Available functions and formats for categorical variables
#'
#' @return Tibble with the available categorical format keys
#'
#' @examples
#' library(PatientProfiles)
#' categoricalFormats()
#'
#' @export
#'
categoricalFormats <- function() {
  return(availableFormat("categorical"))
}

#' Available functions and formats for binary variables
#'
#' @return Tibble with the available binary format keys
#'
#' @examples
#' library(PatientProfiles)
#' binaryFormats()
#'
#' @export
#'
binaryFormats <- function() {
  return(availableFormat("binary"))
}

#' Classify the variables between 5 types: "numeric", "categorical", "binary",
#' "date", or NA.
#'
#' @param x Tibble with different columns.
#'
#' @return Tibble with the variables type and classification
#'
#' @examples
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' variableTypes(x)
#'
#' @export
#'
variableTypes <- function(x) {
  checkmate::assertTibble(x)
  x <- dplyr::tibble(
    variable = colnames(x),
    variable_type = lapply(x, pillar::type_sum) %>% unlist()
  ) %>%
    dplyr::mutate(variable_classification = assertClassification(.data$variable_type, .env$x))
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

#' Detect automatically variables with a certain classification
#'
#' @param x Table
#' @param variableClassification Classification of interest, choice between
#' "numeric", "date", "binary" and "categorical"
#' @param exclude Variables to exclude
#'
#' @return Variables in x with the desired classification
#'
#' @examples
#' library(PatientProfiles)
#' library(tibble)
#' x <- tibble(
#'   person_id = c(1, 2),
#'   start_date = as.Date(c("2020-05-02", "2021-11-19")),
#'   asthma = c(0, 1)
#' )
#' detectVariables(x, "numeric")
#'
#' @export
#'
detectVariables <- function(x,
                            variableClassification,
                            exclude = c(
                              "person_id", "subject_id", "cohort_definition_id",
                              "cohort_name", "strata_name", "strata_level"
                            )) {
  # initial checks
  checkX(x)
  checkVariableClassification(variableClassification)
  checkExclude(exclude)

  # get variable types
  variables <- variableTypes(x) %>%
    dplyr::filter(
      .data$variable_classification == .env$variableClassification
    ) %>%
    dplyr::pull("variable")

  # eliminate excluded variables
  variables <- variables[!(variables %in% excluded)]

  return(variables)
}
