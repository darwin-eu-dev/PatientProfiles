#' Summarise cohort overlap
#'
#' @param cohort  A cohort table in a cdm reference
#'
#' @return A summarised_result
#' @export
#'
#' @examples
#' \donttest{
#' }

summariseCohortOverlap <- function(cohort) {

  # validate inputs
  assertClass(cohort, "cohort_table")
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))

  # add cohort names
  cdm <- omopgenerics::cdmReference(cohort)
  name <- attr(cohort, "tbl_name") # change to omopgenerics::getTableName(cohort)  when og is released

  cohortOrder <- cdm[[name]] |> omopgenerics::settings() |> dplyr::pull(.data$cohort_name)
  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]])

  overlap <- cdm[[name]] |>
    dplyr::group_by(.data$subject_id, .data$cohort_name) |>
    dplyr::mutate(record_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select("subject_id",
                  "record_id",
                  "cohort_name_reference" = "cohort_name") |>
    dplyr::inner_join(cdm[[name]] |>
                        dplyr::group_by(.data$subject_id, .data$cohort_name) |>
                        dplyr::mutate(record_id = dplyr::row_number()) |>
                        dplyr::ungroup() |>
                        dplyr::select("subject_id",
                                      "record_id",
                                      "cohort_name_comparator" = "cohort_name") |>
                        dplyr::distinct(),
                      by = c("subject_id", "record_id"))  |>
    dplyr::group_by(.data$cohort_name_reference,
                    .data$cohort_name_comparator) |>
    dplyr::summarise(
      "number records" = as.character(dplyr::n()),
      "number subjects" = as.character((dplyr::n_distinct("subject_id"))),
      .groups = "drop") |>
    dplyr::collect() |>
    getUniqueCombinations(order = cohortOrder) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("number"),
                        names_to = "variable_name",
                        values_to = "estimate_value") |>
    visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))

  overlap <- overlap |>
    dplyr::bind_rows(omopgenerics::emptySummarisedResult()) |>
    dplyr::mutate(
      result_id = as.integer(1),
      cdm_name = CDMConnector::cdmName(cdm),
      result_type = "cohort_overlap",
      package_name = "PatientProfiles",
      package_version = as.character(utils::packageVersion("PatientProfiles")),
      strata_name = "overall",
      strata_level = "overall",
      variable_level = NA_character_,
      estimate_name = "count",
      estimate_type = "integer",
      additional_name ="overall",
      additional_level = "overall"
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns("summarised_result"))) |>
    omopgenerics::newSummarisedResult()

  return(overlap)
}

getUniqueCombinations <- function(x, order) {
  for (i in 2:length(order)) {
    x <- x |>
      dplyr::anti_join(
        x |>
          dplyr::filter(.data$cohort_name_reference == .env$order[i],
                        .data$cohort_name_comparator %in% .env$order[1:(i-1)]),
        by = colnames(x)
      )
  }
  return(x)
}
