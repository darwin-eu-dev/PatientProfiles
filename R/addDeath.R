

#' Add date of death for individuals
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the age.
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
                         window = c(0, Inf),
                         deathDateName = "date_of_death") {
  addDeath(
    x = x,
    type = "date",
    indexDate = indexDate,
    window = window,
    deathName = deathDateName
  )
}

#' Add days to death for individuals
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the age.
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
                         window = c(0, Inf),
                         deathDaysName = "days_to_death") {
  addDeath(
    x = x,
    type = "days",
    indexDate = indexDate,
    window = window,
    deathName = deathDaysName
  )
}


#' Add flag for death for individuals
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the date to compute the age.
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
                         window = c(0, Inf),
                         deathFlagName = "death") {
  addDeath(
    x = x,
    type = "flag",
    indexDate = indexDate,
    window = window,
    deathName = deathFlagName
  )
}



addDeath <- function(x,
                     type,
                     indexDate,
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
      dplyr::select(!deathName)
  }
  if ("working_record_id" %in% colnames(x)) {
    cli::cli_warn("variable working_record_id already exists and will be dropped (variable of this name is created internally when adding death information)")
    x <- x |>
      dplyr::select(!"working_record_id")
  }

  # get the person variable
  personVariable <- checkX(x)

  # add a record id that we will later use in join
  x <- x |>
    dplyr::mutate(working_record_id = dplyr::row_number())

  # table with death info
  records <- x |>
    dplyr::select(
      personVariable,
      indexDate,
      "working_record_id"
    ) |>
    dplyr::left_join(
      cdm[["death"]] |>
        dplyr::select(!!personVariable := "person_id",
                      "death_date"),
      by = personVariable
    ) %>%
    dplyr::distinct() |>
    dplyr::compute()

  # keep death records if within window
  records <- records %>%
    dplyr::mutate(days_to_death = !!CDMConnector::datediff(indexDate, "death_date"))

  # note if minus inf to inf then we don´t do any filtering
  if (is.infinite(window[1]) & !is.infinite(window[2])) { # minus Inf to number
    records <- records %>%
      dplyr::filter(.data$days_to_death <= !!window[2])
  }
  if (!is.infinite(window[1]) & is.infinite(window[2])) { # number to Inf
    records <- records %>%
      dplyr::filter(.data$days_to_death >= !!window[1])
  }
  if (!is.infinite(window[1]) & !is.infinite(window[2])) { # number to number
    records <- records %>%
      dplyr::filter(
        .data$days_to_death >= !!window[1],
        .data$days_to_death <= !!window[2]
      )
  }

  # people might have multiple death records, so keep first (in window)
  records <- records |>
    dplyr::group_by(
      !!!rlang::syms(personVariable),
      !!!rlang::syms(indexDate),
      .data$working_record_id
    ) |>
    dplyr::summarise(
      death_date = min(.data$death_date, na.rm = TRUE),
      days_to_death = min(.data$days_to_death, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # return selected type
  if (type == "date") {
    records <- records |>
      dplyr::select(
        personVariable,
        "working_record_id",
        "death_date"
      ) |>
      dplyr::rename(!!deathName := "death_date")
  }
  if (type == "days") {
    records <- records |>
      dplyr::select(
        personVariable,
        "working_record_id",
        "days_to_death"
      ) |>
      dplyr::rename(!!deathName := "days_to_death")
  }
  if (type == "flag") {
    records <- records |>
      dplyr::select(
        personVariable,
        "working_record_id"
      ) |>
      dplyr::mutate(!!deathName := 1L)
  }

  # join with target table
  x <- x |>
    dplyr::left_join(records,
      by = c(personVariable, "working_record_id")
    ) |>
    dplyr::select(!"working_record_id")

  if (type == "flag") {
    x <- x |>
      dplyr::mutate(!!deathName :=
        dplyr::if_else(is.na(!!!rlang::syms(deathName)), 0, 1))
  }

  x <- x %>%
    dplyr::compute()

  x
}
