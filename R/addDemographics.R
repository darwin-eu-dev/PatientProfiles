
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
#' @param ageDefaultDay day of the month assigned to individuals
#' with missing day of birth.
#' @param ageImposeMonth TRUE or FALSE. Whether the month of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageImposeDay TRUE or FALSE. Whether the day of the date of birth
#' will be considered as missing for all the individuals.
#' @param ageGroup if not NULL, a list of ageGroup vectors
#' @param sex TRUE or FALSE. If TRUE, sex will be identified
#' @param priorHistory TRUE or FALSE. If TRUE, days of between the start
#' of the current observation period and the indexDate will be calculated
#' @param furureObservation TRUE or FALSE. If TRUE, days between the
#' indexDate and the end of the current observation period will be
#' calculated
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return cohort table with the added demographic information columns
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% addDemographics(cdm)
#' }
#'
addDemographics <- function(x,
                            cdm,
                            indexDate = "cohort_start_date",
                            age = TRUE,
                            ageDefaultMonth = 1,
                            ageDefaultDay = 1,
                            ageImposeMonth = TRUE,
                            ageImposeDay = TRUE,
                            ageGroup = NULL,
                            sex = TRUE,
                            priorHistory = TRUE,
                            furureObservation = TRUE,
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
  checkmate::assertCharacter(indexDate, len = 1,
                             add = errorMessage,
  )
  column1Check <- indexDate %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `indexDate` is not a column of x"
    )
  }
  checkmate::assertList(ageGroup, min.len = 1, null.ok = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]]))
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", 1:length(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  startNames <- names(x)

  person_vaiable <- dplyr::if_else("person_id" %in% names(x),
                                   "person_id", "subject_id")

  personDetails <- cdm[["person"]] %>%
    dplyr::select("person_id",
                  "gender_concept_id",
                  "year_of_birth",
                  "month_of_birth",
                  "day_of_birth") %>%
    dplyr::rename(!!person_vaiable := "person_id")

  if(priorHistory == TRUE || furureObservation == TRUE) {
    # most recent observation period (in case there are multiple)
    obsPeriodDetails <- x %>%
                         dplyr::select(person_vaiable) %>%
                         dplyr::distinct() %>%
                         dplyr::inner_join(cdm[["observation_period"]]  %>%
                                             dplyr::rename(!!person_vaiable := "person_id") %>%
                                             dplyr::select(dplyr::all_of(person_vaiable),
                                                           "observation_period_start_date",
                                                           "observation_period_end_date"),
                                           by = person_vaiable)
  }

  # update dates
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

  personDetails <- personDetails %>%
    dplyr::mutate(year_of_birth1 = as.character(as.integer(.data$year_of_birth)),
                  month_of_birth1 = as.character(as.integer(.data$month_of_birth)),
                  day_of_birth1 = as.character(as.integer(.data$day_of_birth))) %>%
    dplyr::mutate(birth_date = as.Date(
      paste0(
        .data$year_of_birth1,
        "-",
        .data$month_of_birth1,
        "-",
        .data$day_of_birth1
      )
    ))

  x <- x  %>%
    dplyr::left_join(personDetails %>%
                       dplyr::select(dplyr::any_of(c(person_vaiable,
                                                     "birth_date",
                                                     "gender_concept_id",
                                                     "observation_period_start_date",
                                                     "observation_period_end_date"))),
                     by = person_vaiable)

  if(priorHistory == TRUE || furureObservation == TRUE) {
    x <- x  %>%
      dplyr::mutate(row_id = dplyr::row_number())

    # use the closest obs start
    # (in case we have multiple rows and multiple observation periods)
    obsPeriodDetails <- x %>%
      dplyr::left_join(obsPeriodDetails,
                       by = person_vaiable) %>%
      dplyr::filter(.data$observation_period_start_date <=
                      !!rlang::sym(indexDate) &
                      .data$observation_period_end_date >=
                      !!rlang::sym(indexDate)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(person_vaiable, indexDate,
                                                    "row_id")))) %>%
      dplyr::summarise(observation_period_start_date =
                         max(.data$observation_period_start_date, na.rm = TRUE)) %>%
      dplyr::left_join(obsPeriodDetails,
                       by= c(person_vaiable, "observation_period_start_date"))

    x <- x %>%
      dplyr::left_join(obsPeriodDetails,
                       by= c(person_vaiable, indexDate, "row_id")) %>%
      dplyr::select(!"row_id")
  }

  if(age == TRUE) {
    aQ <- ageQuery(indexDate, name = "age")
  } else {
    aQ <- NULL
  }

  if(sex == TRUE) {
    sQ <- sexQuery(name = "sex")
  } else {
    sQ <- NULL
  }

  if(priorHistory == TRUE) {
    pHQ <- priorHistoryQuery(indexDate, name = "prior_history")
  } else {
    pHQ <- NULL
  }

  if(furureObservation == TRUE) {
    fOQ <- futureObservationQuery(indexDate, name = "future_observation")
  } else {
    fOQ <- NULL
  }

  x <- x %>%
    dplyr::mutate(!!!aQ,
                  !!!sQ,
                  !!!pHQ,
                  !!!fOQ)

  x <- x %>%
    dplyr::select(dplyr::all_of(startNames),
                  dplyr::any_of(c("age", "sex",
                                  "prior_history",
                                  "future_observation")))

  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_demographics_added"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }

  if (!is.null(ageGroup)) {
    x <- addCategories(x,
                       cdm = cdm,
                       variable = "age",
                       categories = ageGroup,
                       tablePrefix = tablePrefix)
  }

  return(x)
}



ageQuery <- function(indexDate, name){
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

sexQuery <- function(name){
  return(glue::glue('dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA))') %>%
  rlang::parse_exprs() %>%
  rlang::set_names(glue::glue(name)))
}

priorHistoryQuery <- function(indexDate, name){
return(glue::glue('CDMConnector::datediff("observation_period_start_date",
                      "{indexDate}")') %>%
          rlang::parse_exprs() %>%
          rlang::set_names(glue::glue(name)))
}

futureObservationQuery <- function(indexDate, name){
return(glue::glue('CDMConnector::datediff("{indexDate}",
                          "observation_period_end_date")') %>%
  rlang::parse_exprs() %>%
  rlang::set_names(glue::glue(name)))
}
