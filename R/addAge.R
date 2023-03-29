
#' Add a column to the current tibble with the age of the subject_id at a
#' certain date
#'
#' @param x Tibble with the individuals that we want to add the age. Need to be
#' in cdm.
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param indexDate Variable that points the date to compute the age.
#' @param name Name of the new column that contains age.
#' @param ageGroup List of age groups to be added.
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageDefaultDay day of the month assigned to individuals with missing
#' day of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be
#' considered as missing for all the individuals.
#' @param ageImposeDay Whether the day of the date of birth will be considered
#' as missing for all the individuals.
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return tibble with the age column added
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(PatientProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-01-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
#'   )
#' )
#'
#' person <- tibble::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8532", "8507"),
#'   year_of_birth = c(2000, 1995, NA),
#'   month_of_birth = c(NA, 07, 08),
#'   day_of_birth = c(01, 25, 03)
#' )
#' cdm <- mockDrugUtilisation(person = person, cohort1 = cohort1)
#' addAge(x = cdm[["cohort1"]], cdm = cdm)
#' }
addAge <- function(x,
                   cdm,
                   indexDate = "cohort_start_date",
                   name = "age",
                   ageGroup = NULL,
                   ageDefaultMonth = 1,
                   ageDefaultDay = 1,
                   ageImposeMonth = TRUE,
                   ageImposeDay = TRUE,
                   tablePrefix = NULL) {
  errorMessage <- checkmate::makeAssertCollection()

  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }
  # check cdm exist
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check if indexDate length = 1 and is in table x
  checkmate::assertCharacter(indexDate, len = 1, add = errorMessage)

  indexDateExists <-
    checkmate::assertTRUE(indexDate %in% colnames(x), add = errorMessage)

  if (!isTRUE(indexDateExists)) {
    errorMessage$push(glue::glue('- indexDate "{indexDate}" not found in table'))
  }

  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

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

  # check if default imputation value for month and day are within range allowed
  checkmate::assertInt(ageDefaultMonth, lower = 1, upper = 12)
  checkmate::assertInt(ageDefaultDay, lower = 1, upper = 31)

  # check if ageImposeMonth and compute and tablePrefix are logical
  checkmate::assertLogical(ageImposeMonth, add = errorMessage)
  checkmate::assertLogical(ageImposeDay, add = errorMessage)
  checkmate::assertCharacter(
    tablePrefix,
    len = 1, null.ok = TRUE, add = errorMessage
  )

  ageDefaultMonth <- as.integer(ageDefaultMonth)
  ageDefaultDay <- as.integer(ageDefaultDay)

  checkmate::reportAssertions(collection = errorMessage)

  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  if (name %in% colnames(x)) {
    warning(glue::glue("Column {name} found in x and will be overwrite."))
    x <- x %>% dplyr::select(-dplyr::all_of(name))
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

  # rename so both x and person table contain subject_id
  if ("subject_id" %in% colnames(x) == FALSE) {
    x <- x %>%
      dplyr::rename("subject_id" = "person_id")
  } else {
    x <- x
  }

  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(x %>%
      dplyr::select("subject_id", dplyr::all_of(indexDate)) %>%
      dplyr::distinct(),
    by = "subject_id"
    )

  if (ageImposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$ageDefaultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$ageDefaultMonth,
        .data$month_of_birth
      ))
  }

  if (ageImposeDay == TRUE) {
    person <- person %>%
      dplyr::mutate(day_of_birth = .env$ageDefaultDay)
  } else {
    person <- person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth),
        .env$ageDefaultDay,
        .data$day_of_birth
      ))
  }

  person <- person %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(year_of_birth1 = as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1 = as.character(as.integer(.data$month_of_birth))) %>%
    dplyr::mutate(day_of_birth1 = as.character(as.integer(.data$day_of_birth))) %>%
    dplyr::mutate(birth_date = as.Date(
      paste0(
        .data$year_of_birth1,
        "-",
        .data$month_of_birth1,
        "-",
        .data$day_of_birth1
      )
    )) %>%
    dplyr::mutate(!!name := floor(dbplyr::sql(
      sqlGetAge(
        dialect = CDMConnector::dbms(cdm),
        dob = "birth_date",
        dateOfInterest = indexDate
      )
    ))) %>%
    dplyr::select(dplyr::all_of(c("subject_id", indexDate, name))) %>%
    dplyr::right_join(x, by = c("subject_id", indexDate)) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (is.null(tablePrefix)) {
    person <- person %>%
      CDMConnector::computeQuery()
  } else {
    person <- person %>%
      CDMConnector::computeQuery(
        name = tablePrefix,
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }

  if (!is.null(ageGroup)) {
    person <- addCategories(person, "age", ageGroup, tablePrefix)
  }

  return(person)
}


sqlGetAge <- function(dialect,
                      dob,
                      dateOfInterest) {
  SqlRender::translate(
    SqlRender::render(
      "((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
      dob = dob,
      date_of_interest = dateOfInterest
    ),
    targetDialect = dialect
  )
}
