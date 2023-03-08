# This file is part of CohortProfiles

#' It creates a mock database for testing CohortProfiles package
#'
#' @param x Tibble with the individuals want to get age group, need to be from a cdm object if does not have "age" column
#' @param ageGroup a list of ageGroup vectors. e.g. list(c(0,10),c(11,20)). Default: 0, 150
#' @param cdm Object that contains a cdm reference. Use CDMConnector to obtain a cdm reference.
#' @param ageGroupNames a vector of character. names for age groups, if not provided, default combines characters in ageGroup. e.g. 0;150 if no ageGroup and ageGroupNames provided
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return tibble with the age group column added
#' @export
#'
#' @examples
#' #'
#' \dontrun{
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
#' result <- addAgeGroup(
#'   x = cdm$cohort1, ageGroup = list(c(1, 20), c(21, 30), c(31, 40)), cdm = cdm
#' )
#' }
addAgeGroup <- function(x,
                        ageGroup = NULL,
                        cdm = NULL,
                        ageGroupNames = NULL,
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
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  # if age column not found in x, we need cdm object and addAge function to impute age

  if (!isTRUE("age" %in% colnames(x))) {
    checkmate::assertClass(cdm, classes = "cdm_reference", add = errorMessage)
    print("- age column not found, will be imputed by addAge function")

     if (!isTRUE("condition_start_date" %in% colnames(x))) {
      x <- addAge(x, cdm)
     } else {

      x <- addAge(x, cdm, ageAt = "condition_start_date")
     }
  }


  # check for ageGroup, change it to a list if it is a vector of length 2, push error if other length
  if (isTRUE(checkmate::checkIntegerish(ageGroup))) {
    if (length(ageGroup) == 2) {
      ageGroup <- list(ageGroup)
    } else {
      errorMessage$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }
  }

  # after changing vector to list, we check it is a list with numeric
  checkmate::assertList(ageGroup,
    types = "integerish", null.ok = TRUE,
    add = errorMessage
  )

  # each vector in the list has to have length 2, push error if not
  if (!is.null(ageGroup)) {
    lengthsAgeGroup <- checkmate::assertTRUE(unique(lengths(ageGroup)) == 2,
      add = errorMessage
    )
    if (!isTRUE(lengthsAgeGroup)) {
      errorMessage$push("- ageGroup needs to be a numeric vector of length two,
                        or a list contains multiple length two vectors")
    }

    # first sort ageGroup by first value
    ageGroup <- ageGroup[order(sapply(ageGroup, function(x) x[1], simplify = TRUE), decreasing = FALSE)]

    # check lower bound is smaller than upper bound, allow NA in ageGroup at the moment
    checkAgeGroup <- unlist(lapply(ageGroup, function(x) {
      x[1] <= x[2]
    }))
    checkmate::assertTRUE(all(checkAgeGroup, na.rm = TRUE),
      add = errorMessage
    )

    # check ageGroup overlap
    list1 <- lapply(dplyr::lag(ageGroup), function(x) {
      x[2]
    })
    list1 <- unlist(list1[lengths(list1) != 0])
    list2 <- lapply(dplyr::lead(ageGroup), function(x) {
      x[1]
    })
    list2 <- unlist(list2[lengths(list2) != 0])

    # the first value of the interval needs to be larger than the second value of previous vector
    checkOverlap <- checkmate::assertTRUE(all(list2 - list1 > 0), add = errorMessage)
    if (!isTRUE(checkOverlap)) {
      errorMessage$push("- ageGroup can not have overlapping intervals")
    }
  }




  if (is.null(ageGroup)) {
    ageGroup <- list(c(NA, NA))
  }


  # if ageGroupNames is provided, it needs to be the same length as ageGroup
  checkmate::assertCharacter(ageGroupNames,
    len = length(ageGroup),
    null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)


  # if no ageGroup provided, use 0;150 as ageGroup and ageGroupName
  # if provided ageGroup, but no ageGroupNames provided:
  # ageGroupName will be combination of each range vector
  ageGroup <- lapply(ageGroup, function(xx) {
    xx[1] <- ifelse(is.na(xx[1]), 0, xx[1])
    xx[2] <- ifelse(is.na(xx[2]), 150, xx[2])
    if (is.null(ageGroupNames)) {
      nam <- paste0(xx[1], ";", xx[2])
      xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2], ageGroupNames := nam)
    } else {
      xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2])
    }
    return(xx)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(to_join = 1)

  if (!is.null(ageGroupNames)) {
    ageGroup <- ageGroup %>% dplyr::mutate(ageGroupNames = ageGroupNames)
  }


  # people with NA age will be classified as NA group in ageGroupNames column
  ageGroup <- ageGroup %>%
    dplyr::inner_join(
      dplyr::tibble(
        to_join = 1,
        age = seq(min(ageGroup$age_min), max(ageGroup$age_max))
      ),
      by = "to_join"
    ) %>%
    dplyr::filter(.data$age >= .data$age_min & .data$age <= .data$age_max) %>%
    dplyr::select("age", ageGroupNames)
  x <- x %>%
    dplyr::left_join(ageGroup, by = "age", copy = TRUE)
  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)
}
