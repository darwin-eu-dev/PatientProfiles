# This file is part of CohortProfiles

#' Adds a column with sex information to the individuals of a table
#'
#' @param x cohort table to which add Sex
#' @param cdm object containing the person table with the sex information
#' in gender_concept_id column
#' @param name name of the new column to be added
#' @param tablePrefix The stem for the permanent tables that will
#' be created. If NULL, temporary tables will be used throughout.
#'
#' @return table x with the added column with sex information
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' cdm$cohort %>% addSex(cdm)
#' }
#'
addSex <- function(x,
                   cdm,
                   name = "sex",
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

  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()

  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }

  column2Check <- "person_id" %in% colnames(cdm$person)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$person"
    )
  }

  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )

  checkmate::reportAssertions(collection = errorMessage)

  # Start code

  name <- rlang::enquo(name)
  if("subject_id" %in% colnames(x)){
    x <- cdm[["person"]] %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::inner_join(
        x %>% dplyr::select("subject_id") %>% dplyr::distinct(),
        by = c("subject_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("subject_id", !!name) %>%
      dplyr::right_join(x, by = "subject_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  } else {
    x <- cdm[["person"]] %>%
      dplyr::inner_join(
        x %>% dplyr::select("person_id") %>% dplyr::distinct(),
        by = c("person_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("person_id", !!name) %>%
      dplyr::right_join(x, by = "person_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  }
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
