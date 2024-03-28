#' Summarise cohort overlap
#'
#' @param cohort  A cohort table in a cdm reference.
#' @param cohortId  Vector of cohort definition ids to include, if NULL, all
#' cohort definition ids will be used.
#' @param strata List of the stratifications within each group to be considered.
#' Must be column names in the cohort table provided.
#'
#' @return A summarised result.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' results <- summariseCohortOverlap(cdm$cohort2)
#' CDMConnector::cdmDisconnect(cdm = cdm)
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
  name <- omopgenerics::tableName(cohort)

  if (is.na(name)) {
    cli::cli_abort("Please provide a permanent cohort table.")
  }

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

  overlapOverallData <- cdm[[name]] |>
    dplyr::distinct(.data$subject_id, .data$cohort_name) |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::select(dplyr::all_of(c("subject_id", "cohort_name_reference"))) |>
    dplyr::inner_join(
      cdm[[name]] |>
        dplyr::distinct(.data$subject_id, .data$cohort_name) |>
        dplyr::rename("cohort_name_comparator" = "cohort_name") |>
        dplyr::select(dplyr::all_of(c("subject_id","cohort_name_comparator"))),
      by = "subject_id") |>
    dplyr::compute()

  # overall
  cohort_counts <- omopgenerics::cohortCount(cdm[[name]]) |>
    dplyr::inner_join(omopgenerics::settings(cdm[[name]]),
                      by = "cohort_definition_id") |>
    dplyr::select(cohort_name, number_subjects)
  # get inner join counts
  overlap <- overlapOverallData |>
    dplyr::group_by(.data$cohort_name_reference,
                    .data$cohort_name_comparator) |>
    dplyr::summarise(
      "number_subjects_overlap" = dplyr::n(),
      .groups = "drop") |>
    dplyr::collect() |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
    dplyr::left_join(cohort_counts |> dplyr::rename_with(~paste0(.x, "_reference")), by = "cohort_name_reference") |>
    dplyr::left_join(cohort_counts |> dplyr::rename_with(~paste0(.x, "_comparator")), by = "cohort_name_comparator")

  # overlap counts and percentages
  overlap <- overlap |>
    getOverlapEstimates() |>
    visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
    visOmopResults::uniteStrata(cols = character())

  # strata
  if (!is.null(unlist(strata))) {
    overlapStrataData <- cdm[[name]] |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c("subject_id", "cohort_name", unique(unlist(strata)))))) |>
      dplyr::rename("cohort_name_reference" = "cohort_name") |>
      dplyr::select(dplyr::all_of(c("subject_id", "cohort_name_reference", unique(unlist(strata))))) |>
      dplyr::inner_join(
        cdm[[name]] |>
          dplyr::distinct(dplyr::across(dplyr::all_of(c("subject_id", "cohort_name", unique(unlist(strata)))))) |>
          dplyr::rename("cohort_name_comparator" = "cohort_name") |>
          dplyr::select(dplyr::all_of(c("subject_id","cohort_name_comparator", unique(unlist(strata))))),
        by = c("subject_id", unique(unlist(strata)))) |>
      dplyr::compute()

    overlap <- overlap |>
      dplyr::union_all(
        lapply(strata,
               function(strataName, data = overlapStrataData) {
                 overlap.strata <- data |>
                   dplyr::group_by(dplyr::across(dplyr::all_of(
                     c("cohort_name_reference", "cohort_name_comparator", strataName)))
                   ) |>
                   dplyr::summarise(
                     "number_subjects" = dplyr::n(),
                     .groups = "drop") |>
                   dplyr::collect()
                 cohort_counts.strata <- overlap.strata |>
                   dplyr::filter(.data$cohort_name_reference == .data$cohort_name_comparator) |>
                   dplyr::rename("cohort_name" = "cohort_name_reference") |>
                   dplyr::select(!"cohort_name_comparator")
                 overlap.strata <- overlap.strata |>
                   dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
                   dplyr::rename_with(.fn = ~paste0(.x, "_overlap"), .cols = dplyr::starts_with("number")) |>
                   dplyr::left_join(
                     cohort_counts.strata |>
                       dplyr::rename_with(
                         .fn = ~paste0(.x, "_reference"),
                         .cols = dplyr::all_of(c("cohort_name", "number_subjects"))
                       ),
                     by = c("cohort_name_reference", strataName)
                   ) |>
                   dplyr::left_join(
                     cohort_counts.strata |>
                       dplyr::rename_with(
                         .fn = ~paste0(.x, "_comparator"),
                         .cols = dplyr::all_of(c("cohort_name", "number_subjects"))
                       ),
                     by = c("cohort_name_comparator", strataName)
                   )
                 overlap.strata <- overlap.strata |>
                   getOverlapEstimates() |>
                   visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
                   visOmopResults::uniteStrata(cols = strataName) |>
                   dplyr::select(dplyr::all_of(c("group_name", "group_level", "strata_name", "strata_level", "variable_name",
                                                 "variable_level", "estimate_name", "estimate_type", "estimate_value")))
                 return(overlap.strata)
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
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
    visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))

  noOverlap <- cohortCombinations |>
    dplyr::anti_join(
      overlap |>
        dplyr::distinct(.data$group_name, .data$group_level),
      by = c("group_name", "group_level")
    )

  if (nrow(noOverlap) > 0) {
    overlap <- overlap |>
      dplyr::union(
        noOverlap |>
          dplyr::cross_join(
            overlap |>
              dplyr::distinct(.data$strata_name, .data$strata_level)
          ) |>
          dplyr::cross_join(
            tidyr::expand_grid(variable_name = "number_subjects",
                               variable_level = c("reference", "comparator", "overlap"),
                               estimate_name = c("count", "percentage"),
                               estimate_value = "0"
            ) |>
              dplyr::mutate(estimate_type = dplyr::if_else(.data$estimate_name == "count", "integer", "percentage"))
          )
      )
  }

  overlap <- overlap |>
    dplyr::mutate(
      result_id = as.integer(1),
      cdm_name = CDMConnector::cdmName(cdm),
      result_type = "cohort_overlap",
      package_name = "PatientProfiles",
      package_version = as.character(utils::packageVersion("PatientProfiles")),
      additional_name ="overall",
      additional_level = "overall",
      variable_level = dplyr::if_else(
        .data$variable_level == "reference" | .data$variable_level == "comparator",
        paste0("only_in_", .data$variable_level), .data$variable_level
      )
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns("summarised_result"))) |>
    omopgenerics::newSummarisedResult()

  return(overlap)
}

getOverlapEstimates <- function(x) {
  # overlap counts and percentages
  x |>
    dplyr::mutate(
      number_subjects_reference = .data$number_subjects_reference - .data$number_subjects_overlap,
      number_subjects_comparator = .data$number_subjects_comparator - .data$number_subjects_overlap
    ) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("number"), names_to = "variable_name", values_to = "estimate_value") |>
    dplyr::mutate(
      variable_level = gsub("number_subjects_", "", .data$variable_name),
      variable_name = gsub("_overlap|_reference|_comparator", "", .data$variable_name),
      estimate_name = "count",
      estimate_type = "integer",
      estimate_value = as.character(.data$estimate_value)
    ) |>
    dplyr::union_all(
      x |>
        dplyr::mutate(
          total_subjects = .data$number_subjects_comparator + .data$number_subjects_reference -.data$number_subjects_overlap,
          number_subjects_reference = (.data$number_subjects_reference - .data$number_subjects_overlap)/(.data$total_subjects) * 100,
          number_subjects_comparator = (.data$number_subjects_comparator - .data$number_subjects_overlap)/(.data$total_subjects) * 100,
          number_subjects_overlap = .data$number_subjects_overlap/(.data$total_subjects) * 100
        ) |>
        dplyr::select(!dplyr::all_of(c("total_subjects"))) |>
        tidyr::pivot_longer(cols = dplyr::starts_with("number"), names_to = "variable_name", values_to = "estimate_value") |>
        dplyr::mutate(
          variable_level = gsub("number_subjects_", "", .data$variable_name),
          variable_name = gsub("_overlap|_reference|_comparator", "", .data$variable_name),
          estimate_name = "percentage",
          estimate_type = "percentage",
          estimate_value = as.character(.data$estimate_value)
        )
    )
}
