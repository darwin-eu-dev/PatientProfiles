
#' Add a column to the current tibble with the age of the subject_id at a
#' certain date
#'
#' @param x Tibble with the individuals that we want to add the age. Need to be
#' in cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable that points the date to compute the age.
#' @param ageName Name of the new column that contains age.
#' @param ageGroup List of age groups to be added.
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageDefaultDay day of the month assigned to individuals with missing
#' day of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be
#' considered as missing for all the individuals.
#' @param ageImposeDay Whether the day of the date of birth will be considered
#' as missing for all the individuals.
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return tibble with the age column added
#' @export
#'
#' @examples
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(PatientProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-01-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
#'   )
#' )
#'
#' person <- tibble::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8532", "8507"),
#'   year_of_birth = c(2000, 1995, NA),
#'   month_of_birth = c(NA, 07, 08),
#'   day_of_birth = c(01, 25, 03)
#' )
#' cdm <- mockDrugUtilisation(person = person, cohort1 = cohort1)
#' addAge(x = cdm[["cohort1"]], cdm = cdm)
#' }
addAge <- function(x,
                   cdm,
                   indexDate = "cohort_start_date",
                   ageName = "age",
                   ageGroup = NULL,
                   ageDefaultMonth = 1,
                   ageDefaultDay = 1,
                   ageImposeMonth = TRUE,
                   ageImposeDay = TRUE,
                   tablePrefix = NULL) {
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = indexDate,
      age = TRUE,
      ageName = ageName,
      ageGroup = ageGroup,
      ageDefaultDay = ageDefaultDay,
      ageDefaultMonth = ageDefaultMonth,
      ageImposeDay = ageImposeDay,
      ageImposeMonth = ageImposeMonth,
      sex = FALSE,
      priorHistory = FALSE,
      futureObservation = FALSE,
      tablePrefix = tablePrefix
    )

  return(x)
}


sqlGetAge <- function(dialect,
                      dob,
                      dateOfInterest) {
  SqlRender::translate(
    SqlRender::render(
      "((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
      dob = dob,
      date_of_interest = dateOfInterest
    ),
    targetDialect = dialect
  )
}
