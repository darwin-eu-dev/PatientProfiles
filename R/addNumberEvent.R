#' @export
addNumberEvent <- function(x, cdm, eventTableName, filter = NULL, window = c(NA, NA), name = "number_event", eventAt = "cohort_start_date", eventDate = "cohort_start_date", compute = TRUE) {
  events <- cdm[[eventTableName]]
  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      events <- events %>%
        dplyr::filter_at(
          dplyr::vars(dplyr::all_of(namesFilter[k])),
          dplyr::any_vars(. == !!filter[[k]])
        )
    }
  }
  if ("person_id" %in% colnames(events)) {
    events <- events %>%
      dplyr::rename("subject_id" = "person_id")
  }
  events <- events %>%
    dplyr::select("subject_id","event_date" = dplyr::all_of(eventDate)) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "event_at" = dplyr::all_of(eventAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(date_dif = !!CDMConnector::datediff("event_at", "event_date"))
  if (!is.na(window[1])) {
    events <- events %>%
      dplyr::filter(.data$date_dif >= !!window[1])
  }
  if (!is.na(window[2])) {
    events <- events %>%
      dplyr::filter(.data$date_dif <= !!window[2])
  }
  events <- events %>%
    dplyr::group_by(.data$subject_id, .data$event_at) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!eventAt := "event_at") %>%
    dplyr::right_join(
      x,
      by = c("subject_id", eventAt)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(name),
      ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
}
