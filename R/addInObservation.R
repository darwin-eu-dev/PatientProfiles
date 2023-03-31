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
#' \dontrun{
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
  person_vaiable <- checkX(x)
  checkCdm(cdm)
  checkIndexDate(indexDate,x)
  checkmate::assertCharacter(name, any.missing = FALSE, len = 1)
  name <- checkNewName(name, x)
  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)

  ObsperiodExists <- "observation_period" %in% names(cdm)
  if (!isTRUE(ObsperiodExists)) {
    errorMessage$push(
      "- `observation_period` is not found in cdm"
    )
  }

  cdmObsPeriodCheck <- inherits(cdm$observation_period, "tbl_dbi")
  if (!isTRUE(cdmObsPeriodCheck)) {
    errorMessage$push(
      "- `observation_period` in cdm is not a table "
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertCharacter(indexDate, len = 1,
                             add = errorMessage,
  )

  column1Check <- indexDate %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `indexDate` is not a column of x"
    )
  }

  column2Check <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

  column3Check <- "person_id" %in% colnames(cdm$observation_period)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$observation_period"
    )
  }

  column4Check <- "observation_period_start_date" %in% colnames(cdm$observation_period)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `observation_period_start_date` is not a column of cdm$observation_period"
    )
  }

  column5Check <- "observation_period_end_date" %in% colnames(cdm$observation_period)
  if (!isTRUE(column5Check)) {
    errorMessage$push(
      "- `observation_period_end_date` is not a column of cdm$observation_period"
    )
  }

  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )

  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  name = rlang::enquo(name)

  if("subject_id" %in% colnames(x)) {
    x <- x %>%
      dplyr::left_join(
        cdm$observation_period %>%
          dplyr::select(
            "subject_id" = "person_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = "subject_id"
      ) %>%
      dplyr::mutate(
        !!name := dplyr::if_else(
          .data[[indexDate]] >= .data$observation_period_start_date &
            .data[[indexDate]] <= .data$observation_period_end_date,
          1,
          0
        )
      ) %>%
      dplyr::select(
        -"observation_period_start_date", - "observation_period_end_date"
      )
  } else {
    x <- x %>%
      dplyr::left_join(
        cdm$observation_period %>%
          dplyr::select(
            "person_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = "person_id"
      ) %>%
      dplyr::mutate(
        !!name := dplyr::if_else(
          .data[[indexDate]] >= .data$observation_period_start_date &
            .data[[indexDate]] <= .data$observation_period_end_date,
          1,
          0
        )
      ) %>%
      dplyr::select(
        -"observation_period_start_date", - "observation_period_end_date"
      )
  }

  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)

}
