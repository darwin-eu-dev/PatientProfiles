
#' It adds all demographics columns to the given cohort table: Age, Sex,
#' PriorHistory, and ageGroup if desired
#'
#' @param x cohort table in which to add follow up of individuals
#' @param cdm cdm with the person and observation_period tables to get the info
#' for the individuals in the cohort
#' @param indexDate name of the column with the date at which consider
#' demographic information
#' @param age  TRUE or FALSE. If TRUE, age will be calculated relative to
#' indexDate
#' @param ageDefaultMonth Month of the year assigned to individuals with missing
#' month of birth. By default: 1.
#' @param ageDefaultDay day of the month assigned to individuals with missing day
#' of birth. By default: 1.
#' @param ageImposeMonth Whether the month of the date of birth will be considered
#' as missing for all the individuals. By default: TRUE.
#' @param ageImposeDay Whether the day of the date of birth will be considered as
#' missing for all the individuals. By default: TRUE.
#' @param ageGroup if not NULL, a list of ageGroup vectors
#' @param sex TRUE or FALSE. If TRUE, sex will be identified
#' @param priorHistory TRUE or FALSE. If TRUE, days of prior history will
#' be calculated relative to indexDate
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
  checkmate::assertCharacter(indexDate,
    len = 1,
    add = errorMessage,
  )
  column1Check <- indexDate %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `indexDate` is not a column of x"
    )
  }
  # check for ageGroup, change it to a list if it is a vector of length 2,
  # push error if other length
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
  checkmate::assertCharacter(
    tablePrefix,
    len = 1, null.ok = TRUE, add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # Start code

  if (isTRUE(age)) {
    x <- x %>%
      addAge(
        cdm = cdm,
        indexDate = indexDate,
        ageGroup = ageGroup,
        ageDefaultMonth = ageDefaultMonth,
        ageDefaultDay = ageDefaultDay,
        ageImposeMonth = ageImposeMonth,
        ageImposeDay = ageImposeDay,
        tablePrefix = tablePrefix
      )
  }

  if (isTRUE(sex)) {
    x <- x %>%
      addSex(cdm)
  }

  if (isTRUE(priorHistory)) {
    x <- x %>%
      addPriorHistory(cdm,
        indexDate = indexDate,
        tablePrefix = tablePrefix
      )
  }

  return(x)
}
