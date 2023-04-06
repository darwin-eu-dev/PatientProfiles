
#' It adds all demographics columns to the given cohort table: Age, Sex,
#' PriorHistory, and ageGroup if desired
#'
#' @param x cohort table in which to add follow up of individuals
#' @param cdm cdm with the person and observation_period tables to get the info
#' for the individuals in the cohort
#' @param indexDate name of the column with the date at which consider
#' demographic information
#' @param age TRUE or FALSE. If TRUE, age will be calculated relative to
#' indexDate
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth.
#' @param ageName Age variable name
#' @param ageDefaultDay day of the month assigned to individuals
#' with missing day of birth.
#' @param ageImposeMonth TRUE or FALSE. Whether the month of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageImposeDay TRUE or FALSE. Whether the day of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageGroup if not NULL, a list of ageGroup vectors
#' @param sex TRUE or FALSE. If TRUE, sex will be identified
#' @param sexName Sex variable name
#' @param priorHistory TRUE or FALSE. If TRUE, days of between the start
#' of the current observation period and the indexDate will be calculated
#' @param priorHistoryName Prior history variable name
#' @param futureObservation TRUE or FALSE. If TRUE, days between the
#' indexDate and the end of the current observation period will be
#' calculated
#' @param futureObservationName Future observation variable name
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return cohort table with the added demographic information columns
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>% addDemographics(cdm)
#' }
#'
addDemographics <- function(x,
                            cdm,
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageName = "age",
                            ageDefaultMonth = 1,
                            ageDefaultDay = 1,
                            ageImposeMonth = TRUE,
                            ageImposeDay = TRUE,
                            ageGroup = NULL,
                            sex = TRUE,
                            sexName = "sex",
                            priorHistory = TRUE,
                            priorHistoryName = "prior_history",
                            futureObservation = TRUE,
                            futureObservationName = "future_observation",
                            tablePrefix = NULL) {
  ## change ageDefaultMonth, ageDefaultDay to integer

  if (typeof(ageDefaultMonth) == "character") {
    ageDefaultMonth <- as.integer(ageDefaultMonth)
  }

  if (typeof(ageDefaultDay) == "character") {
    ageDefaultDay <- as.integer(ageDefaultDay)
  }

  ## check for standard types of user error
  person_variable <- checkX(x)
  checkCdm(cdm, c("person", "observation_period"))
  checkmate::assertLogical(age, any.missing = FALSE, len = 1)
  checkmate::assertIntegerish(
    ageDefaultMonth,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertIntegerish(
    ageDefaultDay,
    lower = 1, upper = 31, any.missing = FALSE, len = 1,
    null.ok = !age
  )
  checkmate::assertLogical(ageImposeMonth, any.missing = FALSE, len = 1)
  checkmate::assertLogical(ageImposeDay, any.missing = FALSE, len = 1)
  ageGroup <- checkAgeGroup(ageGroup)
  checkmate::assertLogical(sex, any.missing = FALSE, len = 1)
  checkmate::assertLogical(priorHistory, any.missing = FALSE, len = 1)
  checkmate::assertLogical(futureObservation, any.missing = FALSE, len = 1)
  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)
  checkVariableInX(indexDate, x, !(age | priorHistory | futureObservation))
  if (!(age | sex | priorHistory | futureObservation)) {
    cli::cli_abort("age, sex, priorHistory, futureObservation can not be FALSE")
  }

  # Start code
  startNames <- names(x)

  personDetails <- cdm[["person"]] %>%
    dplyr::select(
      "person_id",
      "gender_concept_id",
      "year_of_birth",
      "month_of_birth",
      "day_of_birth"
    ) %>%
    dplyr::rename(!!person_variable := "person_id")

  if (priorHistory == TRUE || futureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    obsPeriodDetails <- x %>%
      dplyr::select(dplyr::all_of(c(person_variable, indexDate))) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(
        cdm[["observation_period"]] %>%
          dplyr::rename(!!person_variable := "person_id") %>%
          dplyr::select(
            dplyr::all_of(person_variable),
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = person_variable
      ) %>%
      dplyr::filter(.data$observation_period_start_date <=
        .data[[indexDate]] &
        .data$observation_period_end_date >=
          .data[[indexDate]])
  }

  # update dates
  if (age) {
    # impose month
    if (ageImposeMonth == TRUE) {
      personDetails <- personDetails %>%
        dplyr::mutate(month_of_birth = .env$ageDefaultMonth)
    } else {
      personDetails <- personDetails %>%
        dplyr::mutate(month_of_birth = dplyr::if_else(
          is.na(.data$month_of_birth),
          .env$ageDefaultMonth,
          .data$month_of_birth
        ))
    }
    # impose day
    if (ageImposeDay == TRUE) {
      personDetails <- personDetails %>%
        dplyr::mutate(day_of_birth = .env$ageDefaultDay)
    } else {
      personDetails <- personDetails %>%
        dplyr::mutate(day_of_birth = dplyr::if_else(
          is.na(.data$day_of_birth),
          .env$ageDefaultDay,
          .data$day_of_birth
        ))
    }
  }

  personDetails <- personDetails %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(
      year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
      month_of_birth1 = as.character(as.integer(.data$month_of_birth)),
      day_of_birth1 = as.character(as.integer(.data$day_of_birth))
    ) %>%
    dplyr::mutate(birth_date = as.Date(
      paste0(
        .data$year_of_birth1,
        "-",
        .data$month_of_birth1,
        "-",
        .data$day_of_birth1
      )
    ))

  x <- x %>%
    dplyr::left_join(
      personDetails %>%
        dplyr::select(dplyr::any_of(c(
          person_variable,
          "birth_date",
          "gender_concept_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ))),
      by = person_variable
    )

  if (priorHistory == TRUE || futureObservation == TRUE) {
    x <- x %>%
      dplyr::left_join(obsPeriodDetails,
        by = c(person_variable, indexDate)
      )
  }

  if (age == TRUE) {
    aQ <- ageQuery(indexDate, name = ageName)
  } else {
    aQ <- NULL
  }

  if (sex == TRUE) {
    sQ <- sexQuery(name = sexName)
  } else {
    sQ <- NULL
  }

  if (priorHistory == TRUE) {
    pHQ <- priorHistoryQuery(indexDate, name = priorHistoryName)
  } else {
    pHQ <- NULL
  }

  if (futureObservation == TRUE) {
    fOQ <- futureObservationQuery(indexDate, name = futureObservationName)
  } else {
    fOQ <- NULL
  }

  x <- x %>%
    dplyr::mutate(
      !!!aQ,
      !!!sQ,
      !!!pHQ,
      !!!fOQ
    )

  x <- x %>%
    dplyr::select(
      dplyr::all_of(startNames),
      dplyr::any_of(c(
        ageName, sexName,
        priorHistoryName,
        futureObservationName
      ))
    )

  if (is.null(tablePrefix)) {
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(
        name = paste0(
          tablePrefix,
          "_demographics_added"
        ),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }

  if (!is.null(ageGroup)) {
    x <- addCategories(x,
      cdm = cdm,
      variable = ageName,
      categories = ageGroup,
      tablePrefix = tablePrefix
    )
  }

  return(x)
}



ageQuery <- function(indexDate, name) {
  return(glue::glue('floor(dbplyr::sql(
    sqlGetAge(
      dialect = CDMConnector::dbms(cdm),
      dob = "birth_date",
      dateOfInterest = "{indexDate}"
    )
  ))') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

sexQuery <- function(name) {
  return(glue::glue('dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA))') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

priorHistoryQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("observation_period_start_date",
                      "{indexDate}")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}

futureObservationQuery <- function(indexDate, name) {
  return(glue::glue('CDMConnector::datediff("{indexDate}",
                          "observation_period_end_date")') %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name)))
}
