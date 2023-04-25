#' Categorize a numeric variable
#'
#' @param x Table with individuals in the cdm
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a
#' cdm reference.
#' @param variable Target variable that we want to categorize.
#' @param categories List of lists of named categories with lower and upper
#' limit.
#' @param temporary Whether the resultant table should be temporary or
#' permanent.
#'
#' @return tibble with the categorical variable added.
#'
#' @noRd
#'
#' @examples
#' #'
#' \donttest{
#' library(DBI)
#' library(duckdb)
#' library(tibble)
#' library(PatientProfiles)
#' cohort1 <- tibble::tibble(
#'   cohort_definition_id = c("1", "1", "1"),
#'   subject_id = c("1", "2", "3"),
#'   cohort_start_date = c(
#'     as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
#'   ),
#'   cohort_end_date = c(
#'     as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
#'   )
#' )
#'
#' person <- tibble::tibble(
#'   person_id = c("1", "2", "3"),
#'   gender_concept_id = c("8507", "8507", "8507"),
#'   year_of_birth = c(1980, 1970, 2000),
#'   month_of_birth = c(03, 07, NA),
#'   day_of_birth = c(NA, 02, 01)
#' )
#'
#' cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)
#'
#' result <- cdm$cohort1 %>%
#'   addAge(cdm) %>%
#'   addCategories(
#'     variable = "age",
#'     categories = list("age_group" = list(
#'       "0 to 39" = c(0, 39), "40 to 79" = c(40, 79), "80 to 150" = c(80, 150)
#'     ))
#'   )
#' }
addCategories <- function(x,
                          cdm,
                          variable,
                          categories,
                          temporary = TRUE) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    cli::cli_abort("x is not a table")
  }

  checkmate::assertCharacter(variable, len = 1, any.missing = FALSE)
  checkmate::assertTRUE(variable %in% colnames(x))
  checkmate::assertNumeric(dplyr::pull(utils::head(x, 1), variable))
  checkmate::assertList(
    categories,
    types = "list", any.missing = FALSE, unique = TRUE, min.len = 1
  )
  checkmate::assertLogical(temporary, len = 1, any.missing = FALSE)


  for (i in c(1:length(categories))) {
    if (!is.null(names(categories)) && variable == names(categories)[i]){
      cli::cli_warn(paste0(
        "Categories name '",
        names(categories)[i],
        "' already existed in table, the original variable has been overwritten."
      ))
    }
  }

  if (length(unique(names(categories))) < length((names(categories)))){
    cli::cli_abort(
      "Categories have repeated names, please rename the groups.")
  }

  if (is.null(names(categories))) {
    nam <- paste0("category_", 1:length(categories))
  } else {
    nam <- names(categories)
  }

  categoryTibble <- list()
  for (k in seq_along(categories)) {
    categoryTibble[[nam[k]]] <- checkCategory(categories[[k]])
  }

  firstTempTable <- getOption("dbplyr_table_name", 0) + 1
  for (k in seq_along(categories)) {
    categoryTibbleK <- categoryTibble[[k]]
    name <- names(categoryTibble)[k]

    x <- dplyr::mutate(x, variable := .data[[variable]])
    x <- dplyr::mutate(x, !!name := as.character(NA))
    for (i in 1:nrow(categoryTibbleK)) {
      lower <- categoryTibbleK$lower_bound[i]
      upper <- categoryTibbleK$upper_bound[i]
      category <- categoryTibbleK$category_label[i]
      x <- x %>%
        dplyr::mutate(!!name := dplyr::if_else(
          is.na(.data[[name]]) &
            .data$variable >= .env$lower &
            .data$variable <= .env$upper,
          .env$category,
          .data[[name]]
        ))
    }
    x <- dplyr::select(x, -"variable")

    x <- x %>%
      CDMConnector::computeQuery(
        name = CDMConnector:::uniqueTableName(),
        temporary = temporary,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }
  lastTempTable <- getOption("dbplyr_table_name", 0) - 1
  if (isFALSE(temporary) & firstTempTable <= lastTempTable) {
    CDMConnector::dropTable(
      cdm, sprintf("dbplyr_%03i", firstTempTable:lastTempTable)
    )
  }

  return(x)
}
