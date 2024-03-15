#' Create a gt table from a summarisedCharacteristics object.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param summarisedCharacteristics Summary characteristics long table.
#' @param pivotWide variables to pivot wide
#' @param format formats and labels to use
#' @param keepNotFormatted Whether to keep not formatted estimate types
#' @param decimals Decimals per estimate_type
#' @param decimalMark decimal mark
#' @param bigMark big mark
#'
#' @return New table in gt format
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' summariseCharacteristics(
#'   cohort = cdm$cohort1,
#'   ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
#'   tableIntersect = list(
#'     "Visits" = list(
#'       tableName = "visit_occurrence", value = "count", window = c(-365, 0)
#'     )
#'   ),
#'   cohortIntersect = list(
#'     "Medications" = list(
#'       targetCohortTable = "cohort2", value = "flag", window = c(-365, 0)
#'     )
#'   )
#' )
#' }
gtCharacteristics <- function(summarisedCharacteristics,
                              pivotWide = c("CDM Name", "Group", "Strata"),
                              format = c(
                                "N (%)" = "count (percentage%)",
                                "median [min; q25 - q75; max]",
                                "mean (sd)",
                                "median [q25 - q75]",
                                "N" = "count"
                              ),
                              keepNotFormatted = TRUE,
                              decimals = c(default = 0),
                              decimalMark = ".",
                              bigMark = ",") {
  lifecycle::deprecate_stop(
    when = "0.7.0",
    what = "gtCharacteristics()",
    with = "tableCharacteristics()"
  )
}

#' Create a gt table from a summary object.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param summarisedResult A SummarisedResult object.
#' @param long List of variables and specification to long
#' @param wide List of variables and specification to wide
#' @param format formats and labels to use
#' @param keepNotFormatted Whether to keep not formatted estimate types
#' @param decimals Decimals per estimate_type
#' @param decimalMark decimal mark
#' @param bigMark big mark
#'
#' @return A formatted summarisedResult gt object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' cdm$cohort1 %>%
#'   summariseCharacteristics(
#'     ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
#'   )
#' }
#'
gtResult <- function(summarisedResult,
                     long,
                     wide,
                     format = c(
                       "N (%)" = "count (percentage%)",
                       "median [min; q25 - q75; max]",
                       "mean (sd)",
                       "median [q25 - q75]",
                       "N" = "count"
                     ),
                     keepNotFormatted = TRUE,
                     decimals = c(default = 0),
                     decimalMark = ".",
                     bigMark = ",") {
  lifecycle::deprecate_stop(when = "0.7.0", what = "gtResult()")
}
