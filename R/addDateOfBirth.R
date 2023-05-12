#' Add a column with the individual birth date
#'
#' @param x Table in the cdm that contains 'person_id' or 'subject_id'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con().
#' @param name Name of the column to be added with the date of birth
#' @param missingDay Day of the individuals with no or imposed day of birth
#' @param missingMonth Month of the individuals with no or imposed month of
#' birth
#' @param imposeDay Wether to impose day of birth
#' @param imposeMonth Wether to impose month of birth
#'
#' @return The function returns the table x with an extra column that contains
#' the date of birth
#'
#' @export
#'
#' @examples
#' \donttest{
#'   library(PatientProfiles)
#'   db <- DBI::dbConnect(duckdb::duckdb(), CDConnector::eunomia_dir())
#'   cdm <- mockCdm(db, ...)
#'   cdm$person %>%
#'     addDateOfBirth(cdm)
#'   DBI::dbDisconnect(db)
#' }
addDateOfBirth <- function(x,
                           cdm,
                           name = "birth_date",
                           missingDay = 1,
                           misisngMonth = 1,
                           imposeDay = FALSE,
                           imposeMonth = FALSE) {
  # initial checks
  parameters <- checkInputs(
    x, cdm, name, misisngDay, missingMonth, imposeDay, imposeMonth
  )
  # get parameters
  personIdentifier <- parameters$person_identifier
  # impose day
  if (imposeDay) {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = missingDay)
  } else {
    person <- cdm$person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth), .env$misisngDay, .data$day_of_birth)
      )
  }
  # impose month
  if (imposeMonth) {
    person <- person %>%
      dplyr::mutate(month_of_birth = missingMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth), .env$missingMonth, .data$month_of_birth)
      )
  }
  x %>%
    dplyr::left_join(
      person %>%
        dplyr::mutate(!!name := as.Date(paste0(
          .data$year_of_birth, "-", .data$month_of_birth, "-",
          .data$day_of_birth
        ))) %>%
        dplyr::select(personIdentifier = "person_id", dplyr::all_of(name)),
      by = personIdentifier
    )
}
