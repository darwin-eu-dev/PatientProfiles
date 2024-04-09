

#' Add date of death for individuals. Only death within the same observation
#' period than `indeDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathDateName name of the new column to be added.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathDate()
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
addDeathDate <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathDateName = "date_of_death") {
  addDeath(
    x = x,
    value = "date",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathDateName
  )
}

#' Add days to death for individuals. Only death within the same observation
#' period than `indeDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathDaysName name of the new column to be added.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathDays()
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
addDeathDays <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathDaysName = "days_to_death") {
  addDeath(
    x = x,
    value = "days",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathDaysName
  )
}


#' Add flag for death for individuals. Only death within the same observation
#' period than `indeDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathFlagName name of the new column to be added.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathFlag()
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
addDeathFlag <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathFlagName = "death") {
  addDeath(
    x = x,
    value = "flag",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathFlagName
  )
}



addDeath <- function(x,
                     value,
                     indexDate,
                     censorDate,
                     window,
                     deathName) {

  # input validation
  cdm <- omopgenerics::cdmReference(x)
  checkCdm(cdm, tables = "death")
  if(!indexDate %in% colnames(x)){
    cli::cli_abort("{indexDate} variable not found in table")
  }
  if (!is.list(window)) {
    window <- list(window)
  }
  if(length(window) != 1){
    cli::cli_abort("Only one time window can be provided")
  }
  checkWindow(window)
  window <- purrr::list_c(window)
  if (deathName %in% colnames(x)) {
    cli::cli_warn("{deathName} variable already exists and will be overwritten")
    x <- x |>
      dplyr::select(!dplyr::all_of(deathName))
  }

  deathName <- checkSnakeCase(deathName)

  x <- x |>
    .addIntersect(
      tableName = "death",
      value = value,
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      targetStartDate = "death_date",
      targetEndDate = NULL,
      order = "first",
      nameStyle = deathName
    )

  return(x)
}
