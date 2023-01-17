#' @export
addSex <- function(x, cdm, compute = TRUE) {
  x <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      x %>% dplyr::select("subject_id") %>% dplyr::distinct(),
      by = c("subject_id")
    ) %>%
    dplyr::mutate(sex = dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::select("subject_id", "sex") %>%
    dplyr::right_join(x, by = "subject_id") %>%
    dplyr::select(dplyr::all_of(colnames(x)), "sex")
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}
