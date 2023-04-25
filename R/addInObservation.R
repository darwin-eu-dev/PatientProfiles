#' Indicate if a certain record is within the observation period
#'
#' @param x Table with individuals in the cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param name name of the column to hold the result of the enquiry:
#' 1 if the individual is in observation, 0 if not
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return cohort table with the added binary column assessing inObservation
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% addInObservation(cdm)
#' }
#'
addInObservation <- function(x,
                             cdm,
                             indexDate = "cohort_start_date",
                             name = "in_observation",
                             tablePrefix = NULL) {

  ## check for standard types of user error
  person_variable <- checkX(x)
  checkCdm(cdm, c("observation_period"))
  checkVariableInX(indexDate, x)
  checkmate::assertCharacter(name, any.missing = FALSE, len = 1)
  name <- checkNewName(name, x)
  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)
  checkSnakeCase(name, "inObservation")

  # Start code
  name <- rlang::enquo(name)

  x <- x %>%
    addDemographics(cdm,
                    indexDate = indexDate,
                    age = FALSE,
                    sex = FALSE,
                    priorHistory = TRUE,
                    futureObservation = TRUE,
                    tablePrefix = NULL
    ) %>%
    dplyr::mutate(
      !!name := as.numeric(dplyr::if_else(
        is.na(.data$prior_history)| is.na(.data$future_observation)|.data$prior_history < 0|.data$future_observation<0,0,1))) %>%
    dplyr::select(
      -"prior_history", -"future_observation"
    )

  if (is.null(tablePrefix)) {
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(
        name = paste0(tablePrefix, "_with_observation"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }

  return(x)
}
