# Copyright 2022 DARWIN EU (C)
#
# This file is part of DrugUtilisation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Explain function
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'targetCohort' table and all the tables that we want to
#' characterize. It is a compulsory input, no default value is provided.
#' @param targetCohortName Name of the table in the cdm that contains the
#' target cohort that we want to characterize. It is a compulsory input, no
#' default value is provided.
#' @param targetCohortId Cohort definition id for the analyzed target cohorts.
#' It can be a vector or a number. If it is NULL all cohorts are analyzed. By
#' default: NULL.
#' @param temporalWindows Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(NA, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, NA)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "visit_occurrence",
#' "condition_occurrence", "drug_exposure", "procedure_occurrence",
#' "device_exposure", "measurement", "observation", "drug_era", "condition_era"
#' and "specimen". By default: c("condition_occurrence", "drug_era",
#' "procedure_occurrence", "measurement").
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @param minimumCellCount All counts lower than minimumCellCount will be
#' obscured changing its value by NA. 'obscured' column of characterization
#' tibble is TRUE when a count has been obscured. Otherwise it is FALSE.
#'
#' @return The output of this function is a 3 elements list. First
#' ("Characterization") is a reference to a temporal table in the database. It
#' contains the characterization of the desired cohorts of interest. The cohorts
#' of interest are specified using 'targetCohortId' and 'targetCohortName'. The
#' characterized tables are the ones specified in 'tablesToChacaterize'. Second
#' ("temporalWindows") contains the windows used to do the characaterization.
#' Finally "overlap" is also included in the list.
#'
#' @export
#'
#' @examples
largeScaleCharacterization <- function(cdm,
                                       targetCohortName,
                                       targetCohortId = NULL,
                                       temporalWindows = list(
                                         c(NA, -366), c(-365, -91),
                                         c(-365, -31), c(-90, -1), c(-30, -1),
                                         c(0, 0), c(1, 30), c(1, 90),
                                         c(31, 365), c(91, 365), c(366, NA)
                                       ),
                                       tablesToCharacterize = c(
                                         "condition_occurrence", "drug_era",
                                         "procedure_occurrence", "measurement"
                                       ),
                                       overlap = TRUE,
                                       minimumCellCount = 5) {
  get_start_date <- list(
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date"
  )

  get_end_date <- list(
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_end_date",
    "procedure_occurrence" = NULL,
    "device_exposure" = "device_exposure_end_date",
    "measurement" = NULL,
    "observation" = NULL,
    "drug_era" = "drug_era_end_date",
    "condition_era" = "condition_era_end_date",
    "specimen" = NULL
  )

  get_concept <- list(
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id"
  )

  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check targetCohortName
  checkmate::assertCharacter(targetCohortName, len = 1, add = errorMessage)

  # check that targetCohortName point to a table that is a cohort
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[targetCohortName]])),
    add = errorMessage
  )

  # check targetCohortId
  checkmate::assertIntegerish(
    targetCohortId,
    lower = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  # check temporalWindows
  checkmate::assertList(temporalWindows, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(unlist(lapply(temporalWindows, length)) == 2),
    add = errorMessage
  )

  # check tablesToCharacterize
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% c(
      "visit_occurrence", "condition_occurrence", "drug_exposure",
      "procedure_occurrence", "device_exposure", "measurement", "observation",
      "drug_era", "condition_era", "specimen"
    )),
    add = errorMessage
  )

  # overlap
  checkmate::assertLogical(overlap, any.missing = FALSE, add = errorMessage)

  # minimumCellCount
  checkmate::assertCount(minimumCellCount, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  if (length(overlap) > 1) {
    if (length(overlap) != length(tablesToCharacterize)) {
      stop("If length(overlap)>1 then length(overlap) = length(tablesToCharacterize)")
    }
  } else {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }

  # write temporal windows tibble
  temporalWindows <- lapply(temporalWindows, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(
      window_start = x[1], window_end = x[2], window_name = nam
    )
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(window_id = dplyr::row_number()) %>%
    dplyr::select("window_id", "window_name", "window_start", "window_end")

  # filter the cohort and get the targetCohortId if not specified
  if (!is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  } else {
    targetCohort <- cdm[[targetCohortName]]
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  # get the distinct subjects with their observation period
  subjects <- targetCohort %>%
    dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "person_id"
    )

  # for each one of the windows we get which are the subjects contributing to it
  subjects_denominator <- subjects %>%
    dplyr::mutate(dif_start = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_start_date"
    ))) %>%
    dplyr::mutate(dif_end = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_end_date"
    ))) %>%
    dplyr::mutate(to_merge = 1) %>%
    dplyr::inner_join(
      temporalWindows %>%
        dplyr::mutate(to_merge = 1),
      by = "to_merge",
      copy = TRUE
    ) %>%
    dplyr::filter(
      is.na(.data$window_end) | .data$dif_start <= .data$window_end
    ) %>%
    dplyr::filter(
      is.na(.data$window_start) | .data$dif_end >= .data$window_start
    ) %>%
    dplyr::select(
      "person_id", "cohort_start_date", "cohort_end_date", "window_id"
    ) %>%
    dplyr::compute()

  # get the codes observed in each window for each one of the subjects, only
  # events in the observation window will be observed. The result is a
  # temporary table in the database
  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    overlap.k <- overlap[tablesToCharacterize == table_name]
    # get start date depending on the table
    start_date <- get_start_date[[table_name]]
    # get end date depending on the table
    end_date <- get_end_date[[table_name]]
    # get concept id depending on the table
    concept_id <- get_concept[[table_name]]
    # subset the table to the study subjects
    study_table <- cdm[[table_name]] %>%
      dplyr::inner_join(subjects, by = "person_id") %>%
      # rename start date
      dplyr::rename("start_date" = .env$start_date)
    # rename or create end date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      study_table <- study_table %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    study_table <- study_table %>%
      # rename concept id
      dplyr::rename("concept_id" = .env$concept_id) %>%
      # obtain observations inside the observation period only
      dplyr::filter(.data$start_date <= .data$observation_period_end_date) %>%
      dplyr::filter(.data$end_date >= .data$observation_period_start_date) %>%
      # obtain the time difference between the start of the event and the
      # cohort start date
      dplyr::mutate(days_difference_start = dbplyr::sql(CDMConnector::datediff(
        start = "cohort_start_date",
        end = "start_date"
      )))
    # obtain the time difference between the end of the event and the cohort
    # start date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = .data$days_difference_start)
    } else {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = dbplyr::sql(CDMConnector::datediff(
          start = "cohort_start_date",
          end = "end_date"
        )))
    }
    study_table <- study_table %>%
      # merge the table that we want to characterize with all the temporal
      # windows
      dplyr::mutate(to_merge = 1) %>%
      dplyr::inner_join(
        temporalWindows %>%
          dplyr::mutate(to_merge = 1),
        by = "to_merge",
        copy = TRUE
      ) %>%
      # get only the events that start before the end of the window
      dplyr::filter(
        is.na(.data$window_end) |
          .data$days_difference_start <= .data$window_end
      ) %>%
      # get only events that end/start (depending if overlap = TRUE/FALSE) after
      # the start of the window
      dplyr::filter(
        is.na(.data$window_start) |
          .data$days_difference_end >= .data$window_start
      ) %>%
      # get only distinct events per window id
      dplyr::select(
        "person_id", "cohort_start_date", "cohort_end_date", "window_id",
        "concept_id"
      ) %>%
      dplyr::distinct() %>%
      dplyr::compute()

    return(study_table)
  })

  # union all the tables into a temporal table
  for (i in 1:length(characterizedTable)) {
    if (i == 1) {
      characterizedTables <- characterizedTable[[i]] %>%
        dplyr::mutate(table_id = .env$i)
    } else {
      characterizedTables <- characterizedTables %>%
        dplyr::union_all(
          characterizedTable[[i]] %>%
            dplyr::mutate(table_id = .env$i)
        )
    }
  }
  characterizedTables <- characterizedTables %>% dplyr::compute()



  # if we want to summarise the data we count the number of counts for each
  # event, window and table
  for (k in 1:length(targetCohortId)) {
    characterizedCohort <- targetCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::select(
        "person_id" = "subject_id", "cohort_start_date", "cohort_end_date"
      ) %>%
      dplyr::inner_join(
        characterizedTables,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$concept_id, .data$window_id, .data$table_id) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("counts" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])
    denominator <- targetCohort %>%
      dplyr::rename("person_id" = "subject_id") %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::inner_join(
        subjects_denominator,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$window_id) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("in_observation" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])
    if (k == 1) {
      characterizedCohortk <- characterizedCohort
      denominatork <- denominator
    } else {
      characterizedCohortk <- characterizedCohortk %>%
        dplyr::union_all(characterizedCohort)
      denominatork <- denominatork %>%
        dplyr::union_all(denominator)
    }
  }
  characterizedTables <- characterizedCohortk %>%
    dplyr::mutate(obscured_counts = dplyr::if_else(
      .data$counts < .env$minimumCellCount, TRUE, FALSE
    )) %>%
    dplyr::mutate(counts = dplyr::if_else(
      .data$obscured_counts == TRUE,
      as.integer(NA),
      as.integer(.data$counts)
    )) %>%
    dplyr::relocate("cohort_definition_id", .before = "concept_id")
  subjects_denominator <- denominatork %>%
    dplyr::mutate(obscured_in_observation = dplyr::if_else(
      .data$in_observation < .env$minimumCellCount, TRUE, FALSE
    )) %>%
    dplyr::mutate(in_observation = dplyr::if_else(
      .data$obscured_in_observation == TRUE,
      as.integer(NA),
      as.integer(.data$in_observation)
    )) %>%
    dplyr::relocate("cohort_definition_id", .before = "window_id")

  result <- list()
  result$characterization <- characterizedTables
  result$denominator <- subjects_denominator
  result$temporalWindows <- temporalWindows
  result$tablesToCharacterize <- tablesToCharacterize
  result$overlap <- overlap

  return(result)
}
