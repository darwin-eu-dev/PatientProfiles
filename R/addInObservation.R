#' It adds a column to a cohort table indicating whether its individuals are
#' in observation at the desired time
#'
#' @param x cohort table in which the inObservation command wants to be tested
#' @param cdm where the observation_period table is stored
#' @param indexDate name of the column with the dates to test the
#' inObservation command
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
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% inObservation(cdm)
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

  # Start code
  name <- rlang::enquo(name)

  x <- x %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select(
          !!person_variable := "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = person_variable
    ) %>%
    dplyr::mutate(
      !!name := as.numeric(dplyr::if_else(
        .data[[indexDate]] >= .data$observation_period_start_date &
          .data[[indexDate]] <= .data$observation_period_end_date,
        1,
        0
      ))
    ) %>%
    dplyr::select(
      -"observation_period_start_date", -"observation_period_end_date"
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
