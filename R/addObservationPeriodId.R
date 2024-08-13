#' Add the ID associated with current observation period
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the
#' observation flag.
#' @param window window to consider events of.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return cohort table with the added binary column assessing inObservation.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addObservationPeriodId()
#' mockDisconnect(cdm = cdm)
#' }
#'
addObservationPeriodId <- function(x,
                             indexDate = "cohort_start_date",
                             name = NULL) {

  x <- validateX(x)
  name <- validateName(name)
  cdm <- omopgenerics::cdmReference(x)
  xName <- omopgenerics::tableName(x)
  if(!is.na(xName) &&
     omopgenerics::tableName(x) == "observation_period"){
    cli::cli_abort("addObservationPeriodId cannot be used on the observation period table")
  }
  indexDate <- validateIndexDate(indexDate, null = FALSE, x = x)
  personVariable <- c("person_id", "subject_id")
  personVariable <- personVariable[personVariable %in% colnames(x)]

  # drop variable if it already exists
  if("observation_period_id" %in% colnames(x)){
    cli::cli_warn(c("!" = "Existing observation_period_id column will be overwritten"))
    x <- x |>
      dplyr::select(!dplyr::all_of("observation_period_id"))
  }

  # if empty table, return with variable name added
  if(x |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") == 0){
    return(x |>
             dplyr::mutate(observation_period_id = as.integer(NA)))
  }

 currentObsId <- x |>
    dplyr::select(dplyr::all_of(c(personVariable,
                                indexDate))) |>
    dplyr::left_join(
      cdm$observation_period |>
        dplyr::select(dplyr::all_of(c("person_id",
                                    "observation_period_id",
                                    "observation_period_start_date",
                                    "observation_period_end_date"
                                    ))) |>
        dplyr::rename(!!personVariable := "person_id"),
      by = personVariable) |>
    dplyr::filter(
      .data[[indexDate]] <= .data[["observation_period_end_date"]] &&
      .data[[indexDate]] >= .data[["observation_period_start_date"]]) |>
    dplyr::select(dplyr::all_of(c(personVariable,
                                  indexDate,
                                  "observation_period_id")))

  x <- x |>
   dplyr::left_join(currentObsId,
                    by = c(personVariable, indexDate)) |>
    computeTable(name = name)

  x
}
