
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
#' \donttest{
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
  x <- x %>%
    addDemographics(
      cdm = cdm,
      indexDate = NULL,
      age = FALSE,
      ageName = FALSE,
      ageGroup = NULL,
      ageDefaultDay = NULL,
      ageDefaultMonth = NULL,
      ageImposeDay = FALSE,
      ageImposeMonth = FALSE,
      sex = TRUE,
      sexName = sexName,
      priorHistory = FALSE,
      futureObservation = FALSE,
      tablePrefix = tablePrefix
    )

  return(x)
}
