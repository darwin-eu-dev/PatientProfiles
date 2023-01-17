#' @export
addPriorHistory <- function(x, cdm, priorHistoryAt = "cohort_start_date", compute = TRUE) {
  x <- cdm[["observation_period"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "observation_period_start_date"
    ) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", dplyr::all_of(priorHistoryAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(prior_history = CDMConnector::datediff(
      start = "observation_period_start_date",
      end = !!priorHistoryAt
    )) %>%
    dplyr::right_join(
      x,
      by = c("subject_id", priorHistoryAt)
    ) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "prior_history")
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
