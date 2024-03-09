#' Summarise cohort overlap
#'
#' @param cohort  A cohort table in a cdm reference
#' @param cohortId  Vector of cohort definition ids to include, if NULL, all
#' cohort definition ids will be used.
#' @param strata List of the stratifications within each group to be considered.
#' Must be column names in the cohort table provided.
#'
#' @return A summarised_result
#' @export
#'
#' @examples
#' \donttest{
#' }

summariseCohortOverlap <- function(cohort,
                                   cohortId = NULL,
                                   strata = list()) {

  # validate inputs
  assertClass(cohort, "cohort_table")
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkStrata(strata, cohort)

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
      cli::cli_warn("{cohortId[indNot]} {?is/are} not in the cohort table.")
    }
  }

  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::compute()

  overlapData <- cdm[[name]] |>
    dplyr::group_by(.data$subject_id, .data$cohort_name) |>
    dplyr::mutate(record_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::select(dplyr::all_of(c("subject_id",
                                "record_id",
                                "cohort_name_reference",
                                unique(unlist(strata))))) |>
    dplyr::inner_join(cdm[[name]] |>
                        dplyr::group_by(.data$subject_id, .data$cohort_name) |>
                        dplyr::mutate(record_id = dplyr::row_number()) |>
                        dplyr::ungroup() |>
                        dplyr::rename("cohort_name_comparator" = "cohort_name") |>
                        dplyr::select(dplyr::all_of(c("subject_id",
                                                    "record_id",
                                                    "cohort_name_comparator",
                                                    unique(unlist(strata))))),
                      by = c("subject_id", "record_id", unique(unlist(strata)))) |>
    dplyr::compute()

  # overall
  overlap <- overlapData |>
    dplyr::group_by(.data$cohort_name_reference,
                    .data$cohort_name_comparator) |>
    dplyr::summarise(
      "number records" = as.character(dplyr::n()),
      "number subjects" = as.character((dplyr::n_distinct("subject_id"))),
      .groups = "drop") |>
    dplyr::collect() |>
    tidyr::pivot_longer(cols = dplyr::starts_with("number"),
                        names_to = "variable_name",
                        values_to = "estimate_value") |>
    visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
    dplyr::mutate(strata_name = "overall", strata_level = "overall")
    # visOmopResults::uniteStrata() # to change when visOmopResults release is > 0.1.0

   # strata
  if (!is.null(unlist(strata))) {
    overlap <- overlap |>
      dplyr::union_all(
        lapply(strata,
               function(strataName, data = overlapData) {
                 overlapData |>
                   dplyr::group_by(dplyr::across(dplyr::all_of(
                     c("cohort_name_reference", "cohort_name_comparator", strataName)))
                   ) |>
                   dplyr::summarise(
                     "number records" = as.character(dplyr::n()),
                     "number subjects" = as.character((dplyr::n_distinct("subject_id"))),
                     .groups = "drop") |>
                   dplyr::collect() |>
                   tidyr::pivot_longer(cols = dplyr::starts_with("number"),
                                       names_to = "variable_name",
                                       values_to = "estimate_value") |>
                   visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
                   visOmopResults::uniteStrata(cols = strataName)
               }) |>
          dplyr::bind_rows()
      )
  }

  # report even if there is no overlap (number = 0):
  names <- cdm[[name]] |>
    omopgenerics::settings() |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
    dplyr::pull(.data$cohort_name)
  cohortCombinations <- tidyr::expand_grid(
    cohort_name_reference = names,
    cohort_name_comparator = names
    ) |>
    visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))

  noOverlap <- cohortCombinations |>
    dplyr::anti_join(
      overlap |>
        dplyr::distinct(.data$group_name, .data$group_level),
      by = c("group_name", "group_level")
    )

  if (nrow(noOverlap) > 0) {
    overlap <- overlap |>
      dplyr::union_all(
        noOverlap |>
          dplyr::cross_join(
            overlap |>
              dplyr::distinct(.data$strata_name, .data$strata_level)
          ) |>
          dplyr::cross_join(
            dplyr::tibble(
              variable_name = c("number records", "number subjects"),
              estimate_value = "0"
            )
          )
      )
  }

  overlap <- overlap |>
    dplyr::bind_rows(omopgenerics::emptySummarisedResult()) |>
    dplyr::mutate(
      result_id = as.integer(1),
      cdm_name = CDMConnector::cdmName(cdm),
      result_type = "cohort_overlap",
      package_name = "PatientProfiles",
      package_version = as.character(utils::packageVersion("PatientProfiles")),
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

