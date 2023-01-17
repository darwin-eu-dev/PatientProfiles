#' @export
addAgeGroup <- function(x, ageGroup = NULL, name = "age_group", compute = TRUE) {
  ageGroup <- lapply(ageGroup, function(xx) {
    xx[1] <- ifelse(is.na(xx[1]), 0, xx[1])
    xx[2] <- ifelse(is.na(xx[2]), 150, xx[2])
    nam <- paste0(xx[1], ";", xx[2])
    xx <- dplyr::tibble(age_min = xx[1], age_max = xx[2], !!name := nam)
    return(xx)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(to_join = 1)
  ageGroup <- ageGroup %>%
    dplyr::inner_join(
      dplyr::tibble(
        to_join = 1,
        age = seq(min(ageGroup$age_min), max(ageGroup$age_max))
      ),
      by = "to_join"
    ) %>%
    dplyr::filter(.data$age >= .data$age_min & .data$age <= .data$age_max) %>%
    dplyr::select(dplyr::all_of(c("age", name)))
  x <- x %>%
    dplyr::left_join(ageGroup, by = "age", copy = TRUE)
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
