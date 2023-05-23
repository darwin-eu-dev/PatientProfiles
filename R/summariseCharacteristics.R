#' Summarise the characteristics of different individuals
#'
#' @param table Table with different records
#' @param strata List of the stratifications to be considered.
#' @param variables List of the different groups of variables, by default they
#' are automatically classified.
#' @param functions List of functions to be applied to each one of the group of
#' variables.
#' @param spressCellCount Minimum count of records to report results.
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
                                        numericVariables = detectVariables(x, "numeric"),
                                        dateVariables = detectVariables(x, "date"),
                                        binaryVariables = detectVariables(x, "binary"),
                                        categoricalVariables = detectVariables(x, "categorical")
                                      ),
                                      functions = lsit(
                                        numericVariables = c("median", "q25", "q75"),
                                        dateVariables = c("median", "q25", "q75"),
                                        binaryVariables = c("count", "%"),
                                        categoricalVariables = c("count", "%")
                                      ),
                                      spressCellCount = 5,
                                      bigMark = ",",
                                      decimalMark = ".",
                                      significantDecimals = 2) {
  # initial checks
  checkTable(table)
  checkStrata(strata, table)
  checkVariables(variables, table)
  checkFunctions(functions, table)
  checkSameNames(variables, functions)
  checkSuperssCellCount(spressCellCount)
  checkBigMark(bigMark)
  checkDecimalMark(decimalMark)
  checkSignificantDecimals(significantDecimals)
}
