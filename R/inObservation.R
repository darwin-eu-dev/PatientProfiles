#' @export
inObservation <- function(x, cdm) {
  x %>%
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::mutate(
      in_observation = dplyr::if_else(
        .data$cohort_start_date >= .data$observation_period_start_date &
          .data$cohort_start_date <= .data$observation_period_end_date,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(
      -"observation_period_start_date", - "observation_period_end_date"
    ) %>%
    dplyr::compute()
}
