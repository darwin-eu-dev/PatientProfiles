#' It creates categories from a numeric variable.
#'
#' @param x Table in the database.
#' @param cdm cdm reference
#' @param variable Target variable that we want to categorize.
#' @param categories List of lists of named categories with lower and upper
#' limit.
#' @param tablePrefix The stem for the permanent tables that will be created. If
#' NULL, temporary tables will be used throughout.
#'
#' @return tibble with the categorical variable added.
#'
#' @export
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
                          tablePrefix = NULL) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    stop("x is not a table")
  }

  checkmate::assertCharacter(variable, len = 1, any.missing = FALSE)
  checkmate::assertTRUE(variable %in% colnames(x))
  checkmate::assertNumeric(dplyr::pull(head(x, 1), variable))

  checkmate::assertList(
    categories,
    types = "list", any.missing = FALSE, unique = TRUE, min.len = 1
  )

  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)

  if (is.null(names(categories))) {
    nam <- rep("", length(categories))
  } else {
    nam <- names(categories)
  }

  categoryTibble <- list()
  for (k in seq_along(categories)) {
    categoryTibble[[nam[k]]] <- checkCategory(categories[[k]])
  }

  for (k in seq_along(categories)) {
    categoryTibbleK <- categoryTibble[[k]]
    name  <- names(categoryTibble)[k]
    x <- dplyr::mutate(x, !!name := as.character(NA))
    for (i in 1:nrow(categoryTibbleK)) {
      lower <- categoryTibbleK$lower_bound[i]
      upper <- categoryTibbleK$upper_bound[i]
      category <- categoryTibbleK$category_label[i]
      x <- x %>%
        dplyr::mutate(!!name := dplyr::if_else(
          is.na(.data[[name]]) &
            .data[[variable]] >= .env$lower &
            .data[[variable]] <= .env$upper,
          .env$category,
          .data[[name]]
        ))
    }
    if (!is.null(tablePrefix)) {
      x <- CDMConnector::computeQuery(
        x,
        name = paste0(tablePrefix,
                      "_categories_added"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
    } else {
      x <- CDMConnector::computeQuery(x)
    }
  }

  return(x)
}

#' @noRd
checkCategory <- function (category) {
  checkmate::assertList(
    category, types = "integerish", any.missing = FALSE, unique = TRUE,
    min.len = 1
  )

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      stop(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    return(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    stop("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(category_label = names(.env$category)) %>%
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      paste0(.data$lower_bound, " to ", .data$upper_bound),
      .data$category_label
    )) %>%
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if (nrow(result) > 1) {
    lower <- result$lower_bound[2:nrow(result)]
    upper <- result$upper_bound[1:(nrow(result)-1)]
    if (!all(lower > upper)) {
      stop("There can not be overlap between categories")
    }
  }
  return(result)
}
