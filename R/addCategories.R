#' It creates categories from a numeric variable.
#'
#' @param x Table in the database.
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
#' library(CohortProfiles)
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
#' cdm <- mockCohortProfiles(person = person, cohort1 = cohort1)
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
                          variable,
                          categories,
                          tablePrefix = NULL) {
  if (!isTRUE(inherits(x, "tbl_dbi"))) {
    stop("x is not a table")
  }

  checkmate::assertCharacter(variable, len = 1, any.missing = FALSE)
  checkmate::assertTRUE(variable %in% colnames(x))

  checkmate::assertList(
    categories,
    types = "list", any.missing = FALSE, unique = TRUE, min.len = 1
  )

  checkmate::assertCharacter(tablePrefix, len = 1, null.ok = TRUE)

  if (is.null(names(categories))) {
    names(categories) <- rep("", length(categories))
  }

  categoryTibble <- list()
  for (k in seq_along(categories)) {
    checkmate::assertList(
      categories[[k]],
      types = "integerish", any.missing = FALSE, unique = TRUE,
      min.len = 1, max.len = 2
    )

    categoriesK <- categories[[k]]

    if (is.null(names(categoriesK))) {
      names(categoriesK) <- rep("", length(categoriesK))
    }

    # check length
    categoriesK <- lapply(categoriesK, function(x) {
      if (length(x) == 1) {
        x <- c(x, x)
      }
      return(x)
    })

    # check lower bound is smaller than upper bound
    checkLower <- unlist(lapply(categoriesK, function(x) {
      x[1] <= x[2]
    }))
    if (!(all(checkLower))) {
      stop("Lower bound should be equal or smaller for all categories")
    }

    # built tibble
    categoryTibbleK <- lapply(categoriesK, function(x) {
      dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(category_label = names(.env$catgeoriesK)) %>%
      dplyr::mutate(category_label = dplyr::if_else(
        .data$category_label == "",
        paste0(.data$lower_bound, " to ", .data$upper_bound),
        .data$category_label
      )) %>%
      dplyr::mutate(category_name = !!names(categories)[k]) %>%
      dplyr::mutate(category_name = dplyr::if_else(
        .data$category_name == "",
        paste0("category_", k),
        .data$category_name
      )) %>%
      dplyr::relocate("category_name") %>%
      dplyr::arrange(.data$lower_bound)

    # check overlap
    if (nrow(categoryTibbleK) > 1) {
      lower <- categoryTibbleK$lower_bound[2:nrow(categoryTibbleK)]
      upper <- categoryTibbleK$upper_bound[1:(nrow(categoryTibbleK)-1)]
      if (!all(lower > upper)) {
        stop("There can not be overlap between categories")
      }
    }

    # merge with all
    categoryTibble[[unique(categoryTibbleK$category_name)]] <- categoryTibbleK
  }

  for (k in seq_along(categories)) {
    categoryTibbleK <- categoryTibble[[k]]
    name  <- names(categoryTibble)[k]
    x <- dplyr::mutate(!!name := as.character(NA))
    for (i in 1:nrow(categoryTibbleK)) {
      x <- x %>%
        dplyr::mutate(!!name := dplyr::if_else(
          is.na(.data[[name]]) &
            .data[[variable]] >= .env$categoryTibbleK$lower_bound[i] &
            .data[[variable]] <= .env$categoryTibbleK$upper_bound[i],
          .env$categoryTibbleK$category_label[i],
          .data[[name]]
        ))
    }
    if (!is.null(tablePrefix)) {
      x <- CDMConnector::computeQuery(x, tablePrefix, FALSE)
    } else {
      x <- CDMConnector::computeQuery(x)
    }
  }

  return(x)
}
