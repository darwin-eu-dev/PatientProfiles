#' Summarise cohort timing
#'
#' @param cohort  A cohort table in a cdm reference.
#' @param cohortId  Vector of cohort definition ids to include, if NULL, all
#' cohort definition ids will be used.
#' @param strata List of the stratifications within each group to be considered.
#' Must be column names in the cohort table provided.
#' @param restrictToFirstEntry If TRUE only an individual's first entry per
#' cohort will be considered. If FALSE all entries per individual will be
#' considered.
#' @param timing Summary statistics for timing.
#' @param density Get data for density plot.
#'
#' @return A summarised result.
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' results <- summariseCohortTiming(cdm$cohort2)
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
summariseCohortTiming <- function(cohort,
                                  cohortId = NULL,
                                  strata = list(),
                                  restrictToFirstEntry = TRUE,
                                  timing = c("min", "q25",
                                             "median","q75",
                                             "max"),
                                  density = FALSE){

  # validate inputs
  assertClass(cohort, "cohort_table")
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkStrata(strata, cohort)
  checkmate::assertTRUE(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %in% colnames(cohort)))
  checkmate::assertLogical(restrictToFirstEntry, any.missing = FALSE, len = 1, null.ok = FALSE)
  checkmate::assertCharacter(timing, any.missing = FALSE, null.ok = FALSE)

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
      cli::cli_warn("{paste0(cohortId[indNot], collapse = ', ')} {?is/are} not in the cohort table.")
    }
  }
  cdm[[name]] <- PatientProfiles::addCohortName(cdm[[name]]) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)

  if(isTRUE(restrictToFirstEntry)){
    # to use cohortConstructor once released
    # cdm[[name]] <- cdm[[name]] |>
    #   restrictToFirstEntry()
    cdm[[name]] <- cdm[[name]]  |>
      dplyr::group_by(.data$subject_id,.data$cohort_definition_id) |>
      dplyr::filter(.data$cohort_start_date == min(.data$cohort_start_date, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  strataCols <- unlist(strata)
  # should we use addCohortIntersectDate instead to avoid potentially large number of rows?
  cohort_timings <- cdm[[name]] |>
    dplyr::rename("cohort_name_reference" = "cohort_name") |>
    dplyr::select(dplyr::all_of(c(strataCols, "cohort_name_reference",
                                  "cohort_start_date", "cohort_end_date",
                                  "subject_id"))) |>
    dplyr::inner_join(
      cdm[[name]] |>
        dplyr::rename_with(~ paste0(.x, "_comparator"),
                           .cols = c("cohort_definition_id", "cohort_start_date",
                                     "cohort_end_date", "cohort_name")) |>
        dplyr::select(dplyr::all_of(c(strataCols, "cohort_name_comparator",
                                      "cohort_start_date_comparator", "cohort_end_date_comparator",
                                      "subject_id"))),
      by = c("subject_id", unique(strataCols))) |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) %>%
    dplyr::mutate(diff_days = !!CDMConnector::datediff("cohort_start_date",
                                                       "cohort_start_date_comparator",
                                                       interval = "day")) |>
    dplyr::collect()

  timingsResult <- omopgenerics::emptySummarisedResult()

  if (nrow(cohort_timings) > 0 & length(timing) > 0) {
    timingsResult <- cohort_timings |>
      summariseResult(group = list(c("cohort_name_reference", "cohort_name_comparator")),
                      includeOverallGroup = FALSE,
                      strata = strata,
                      variables = list(diff_days = "diff_days"),
                      estimates = list(diff_days = timing)) |>
      dplyr::mutate(result_type = "cohort_timing",
                    cdm_name = CDMConnector::cdmName(cdm))
  }

  if (density & nrow(cohort_timings) > 0) {
    forDensity <- cohort_timings |>
      visOmopResults::uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator"))
    forDensity <- lapply(c(list(character(0)), strata), function(levels, data = forDensity) {
      data |> visOmopResults::uniteStrata(cols = levels)
    }) |>
      dplyr::bind_rows() |>
      dplyr::select(!dplyr::all_of(c(strataCols)))

    groups <- unique(forDensity$group_level)
    timingDensity <- NULL
    for (gLevel in groups) {
      # filter group comparison
      gData <- forDensity |> dplyr::filter(.data$group_level == .env$gLevel)
      strataNames <- unique(gData$strata_name)
      for (sName in strataNames) {
        # filter strata name
        sNameData <- gData |> dplyr::filter(.data$strata_name == .env$sName)
        strataLevels <- unique(sNameData$strata_level)
        # compute density for each strata level
        for (sLevel in strataLevels) {
          sLevelData <- sNameData |> dplyr::filter(.data$strata_level == .env$sLevel)
          if (nrow(sLevelData) > 2) {
            timingDensity <- timingDensity |>
              dplyr::union_all(
                getDensityData(sLevel, sLevelData) |>
                  dplyr::mutate(
                    group_level = gLevel,
                    strata_name = sName
                  )
              )
          }
        }
      }
    }
    timingsResult <- timingsResult |>
      dplyr::union_all(
        timingDensity |>
          dplyr::bind_rows() |>
          dplyr::mutate(
            result_id = as.integer(1),
            cdm_name = CDMConnector::cdmName(cdm),
            result_type = "cohort_timing",
            package_name = "PatientProfiles",
            package_version = as.character(utils::packageVersion("PatientProfiles")),
            group_name = "cohort_name_reference &&& cohort_name_comparator",
            variable_name = "density",
            estimate_type = "numeric",
            additional_name ="overall",
            additional_level = "overall"
          )
      ) |>
      dplyr::select(dplyr::all_of(omopgenerics::resultColumns("summarised_result")))
  }

  # add settings
  if (nrow(timingsResult) > 0) {
    timingsResult <- timingsResult |>
      dplyr::union_all(
        dplyr::tibble(
          result_id = as.integer(1),
          "cdm_name" = omopgenerics::cdmName(cdm),
          "result_type" = "cohort_timing",
          "package_name" = "PatientProfiles",
          "package_version" = as.character(utils::packageVersion("PatientProfiles")),
          "group_name" = "overall",
          "group_level" = "overall",
          "strata_name" = "overall",
          "strata_level" = "overall",
          "variable_name" = "settings",
          "variable_level" = NA_character_,
          "estimate_name" = "restrict_to_first_entry",
          "estimate_type" = "logical",
          "estimate_value" = as.character(restrictToFirstEntry),
          "additional_name" = "overall",
          "additional_level" = "overall"
        )
      ) |>
      omopgenerics::newSummarisedResult()
  }

  return(timingsResult)
}


getDensityData <- function(sLevel, data) {
  dStrata <- data$diff_days[data$strata_level == sLevel]
  d <- stats::density(dStrata)
  densityResult <- dplyr::tibble(
    variable_level = as.character(1:length(d$x)),
    estimate_name = "x",
    estimate_value = as.character(d$x)
  ) |>
    dplyr::union_all(
      dplyr::tibble(
        variable_level = as.character(1:length(d$y)),
        estimate_name = "y",
        estimate_value = as.character(d$y)
      )
    ) |>
    dplyr::mutate(
      strata_level = sLevel
    )
  return(densityResult)
}
