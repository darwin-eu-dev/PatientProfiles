
#' Adds a column with sex information to the individuals of a table
#'
#' @param x cohort table to which add Sex
#' @param cdm object containing the person table with the sex information
#' in gender_concept_id column
#' @param sexName name of the new column to be added
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return table x with the added column with sex information
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% addSex(cdm)
#' }
#'
addSex <- function(x,
                   cdm,
                   sexName = "sex",
                   tablePrefix = NULL) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }

  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  PersonExists <- "person" %in% names(cdm)
  if (!isTRUE(PersonExists)) {
    errorMessage$push(
      "- `person` is not found in cdm"
    )
  }
  PersonCheck <- inherits(cdm$person, "tbl_dbi")
  if (!isTRUE(PersonCheck)) {
    errorMessage$push(
      "- table `person` is not of the right type"
    )
  }

  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

  column2Check <- "person_id" %in% colnames(cdm$person)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$person"
    )
  }

  checkmate::assertCharacter(sexName, len = 1,
                             add = errorMessage,
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code

  x <- x %>%
    addDemographics(cdm = cdm,
                    indexDate = NULL,
                    age = FALSE,
                    ageName = FALSE,
                    ageGroup = NULL,
                    ageDefaultDay = FALSE,
                    ageDefaultMonth = FALSE,
                    ageImposeDay =  FALSE,
                    ageImposeMonth = FALSE,
                    sex = TRUE,
                    sexName = sexName,
                    priorHistory = FALSE,
                    furureObservation = FALSE,
                    tablePrefix = tablePrefix
    )

  return(x)
}
