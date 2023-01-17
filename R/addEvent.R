#' @export
addEvent <- function(x, cdm, eventTableName, eventId, window = c(NA, NA), name = "event", eventDate = "cohort_start_date", eventAt = "cohort_start_date", order = "first", compute = TRUE) {
  eventTable <- cdm[[eventTableName]]
  if (!is.null(eventId)) {
    eventTable <- eventTable %>% dplyr::filter(.data$cohort_definition_id == .env$eventId)
  }
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", eventAt))))
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "all") {
    xx <- xx %>%
      dbplyr::window_order(.data$event_date) %>%
      dplyr::mutate(nam = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nam = paste0(.env$name, "_", .data$nam)) %>%
      tidyr::pivot_wider(names_from = "nam", values_from = "event_date")
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}
