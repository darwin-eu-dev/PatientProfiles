#' Summarise cohort timing
#'
#' @param cohort  A cohort table in a cdm reference
#' @param cohortId  Vector of cohort definition ids to include, if NULL, all
#' cohort definition ids will be used.
#' @param strata List of the stratifications within each group to be considered.
#' Must be column names in the cohort table provided.
#' @param restrictToFirstEntry If TRUE only an individual's first entry per
#' cohort will be considered. If FALSE all entries per individual will be
#' considered
#' @param timing Summary statistics for timing.
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#' }
#'
summariseCohortTiming <- function(cohort,
                                  cohortId = NULL,
                                  strata = list(),
                                  restrictToFirstEntry = TRUE,
                                  timing = c("min", "q25",
                                             "median","q75",
                                             "max"),
                                  densityEstimates = FALSE){

  # validate inputs
  assertClass(cohort, "cohort_table")
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkStrata(strata, cohort)
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))
  checkmate::assertLogical(restrictToFirstEntry, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::assertCharacter(timing, any.missing = FALSE, null.ok = FALSE)

  # add cohort names
  cdm <- omopgenerics::cdmReference(cohort)
  name <- attr(cohort, "tbl_name") # change to omopgenerics::getTableName(cohort)  when og is released

  ids <- cdm[[name]] |>
    omopgenerics::settings() |>
    dplyr::pull(.data$cohort_definition_id)

  if (is.null(cohortId)) {
    cohortId <- ids
  } else {
    indNot <- which(!cohortId %in% ids)
    if (length(indNot)>0) {
      cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table.")
    }
  }
  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if(isTRUE(restrictToFirstEntry)){
    # to use cohortConstructor once released
    # cdm[[name]] <- cdm[[name]] |>
    #   restrictToFirstEntry()
    cdm[[name]] <- cdm[[name]]  |>
      dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
      dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # should we use addCohortIntersectDate instead to avoid potentially large number of rows?
  cohort_timings <- cdm[[name]] |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::inner_join(cdm[[name]] |>
                        dplyr::rename_with(~ paste0(.x, "_comparator"),
                                           .cols = c("cohort_definition_id", "cohort_start_date", "cohort_end_date", "cohort_name")),
                      by = c("subject_id", unique(unlist(strata)))) |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) %>%
    dplyr::mutate(diff_days = !!CDMConnector::datediff("cohort_start_date",
                                                       "cohort_start_date_comparator",
                                                       interval = "day")) |>
    dplyr::collect()

  if (nrow(cohort_timings) > 0) {
    cohort_timings <- cohort_timings |>
      dplyr::mutate("cohort_name_reference &&& cohort_name_comparator" =
                      paste0(.data$cohort_name_reference, " &&& ", .data$cohort_name_comparator)) |>
      summariseResult(group = list("cohort_name_reference &&& cohort_name_comparator"),
                      includeOverallGroup = FALSE,
                      strata = strata,
                      variables = list(diff_days = "diff_days"),
                      functions = list(diff_days = timing)) |>
      dplyr::mutate(result_type = "cohort_timing",
                    cdm_name = CDMConnector::cdmName(cdm))
  }

  return(cohort_timings)
}
