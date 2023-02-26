# This file is part of CohortProfiles

#' It creates a mock database for testing CohortProfiles package
#'
#' @param x ...
#' @param cdm ...
#' @param window ...
#'
#' @return
#' @export
#'
#' @examples
addNumberVisits <- function(x,
                            cdm,
                            window = c(-365, 0)) {
  cdm[["visit_occurrence"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "visit_concept_id", "visit_start_date"
    ) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[1]) <=
        .data$visit_start_date
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[2]) >=
        .data$visit_start_date
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date, .data$cohort_end_date
    ) %>%
    dplyr::summarise(number_visits = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(
      x,
      by = c("subject_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "number_visits")
}
