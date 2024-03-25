# Copyright 2023 DARWIN EU (C)
#
# This file is part of PatientProfiles
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

#' It creates columns to indicate overlap information between a table and a
#' concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetStartDate Event start date to use for the intersection.
#' @param targetEndDate Event end date to use for the intersection.
#' @param order last or first date to use for date/days calculations.
#' @param value Choices between c("value", "flag", "days", "date").
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#'  concept <- dplyr::tibble(
#'   concept_id = c(1125315),
#'   domain_id = "Drug",
#'   vocabulary_id = NA_character_,
#'   concept_class_id = "Ingredient",
#'   standard_concept = "S",
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01"),
#'   invalid_reason = NA_character_
#'  ) %>%
#'  dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#'  cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' result <- cdm$cohort1 %>%
#'  addConceptIntersect(
#'   conceptSet = list("acetaminophen"=1125315)
#'   ) %>%
#'  dplyr::collect()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#'  }
#'
addConceptIntersect <- function(x,
                                conceptSet,
                                indexDate = "cohort_start_date",
                                censorDate = NULL,
                                window = list(c(0, Inf)),
                                targetStartDate = "event_start_date",
                                targetEndDate = "event_end_date",
                                order = "first",
                                value = c("flag", "count", "date", "days"),
                                nameStyle = "{value}_{concept_name}_{window_name}") {
  lifecycle::deprecate_warn(
    when = "0.6.0",
    what = "addConceptIntersect()",
    details = c(
      "please use the specific functions instead:",
      "*" = "addConceptIntersectFlag()", "*" = "addConceptIntersectCount()",
      "*" = "addConceptIntersectDate()", "*" = "addConceptIntersectDays()"
    )
  )
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = order,
    value = value,
    nameStyle = nameStyle
  )
}

.addConceptIntersect <- function(x,
                                 conceptSet,
                                 indexDate = "cohort_start_date",
                                 censorDate = NULL,
                                 window,
                                 targetStartDate = "event_start_date",
                                 targetEndDate = "event_end_date",
                                 order = "first",
                                 value,
                                 nameStyle = "{value}_{concept_name}_{window_name}") {
  # initial checks
  omopgenerics::newCodelist(conceptSet)
  assertChoice(targetStartDate, choices = c("event_start_date", "event_end_date"), length = 1)
  assertChoice(targetEndDate, choices = c("event_start_date", "event_end_date"), length = 1, null = TRUE)

  cdm <- omopgenerics::cdmReference(x)
  tablePrefix <- omopgenerics::tmpPrefix()

  nameStyle <- gsub("\\{concept_name\\}", "\\{id_name\\}", nameStyle)

  # concepts table
  conceptsTable <- getConceptsTable(conceptSet)
  nm <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = conceptsTable)

  # ids
  conceptSetId <- conceptSetId(conceptSet)

  conceptSet <- list("mc" = 4112343)

  # subset table
  cdm[[nm]] <- subsetTable(cdm[[nm]]) |>
    dplyr::compute(name = nm, temporary = FALSE)
  attr(x, "cdm_reference") <- cdm
  x <- x |>
    .addIntersect(
      tableName = nm,
      value = value,
      filterVariable = "concept_set_id",
      filterId = conceptSetId$concept_set_id,
      idName = conceptSetId$concept_set_name,
      window = window,
      order = order,
      indexDate = indexDate,
      censorDate = censorDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      nameStyle = nameStyle
    )

  # drop intermediate tables
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(x)
}
getConceptsTable <- function(conceptSet) {
  purrr::map(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "concept_set_name") |>
    dplyr::inner_join(conceptSetId(conceptSet), by = "concept_set_name") |>
    dplyr::select("concept_id" = "value", "concept_set_id")
}
conceptSetId <- function(conceptSet) {
  dplyr::tibble(
    "concept_set_name" = names(conceptSet),
    "concept_set_id" = as.integer(seq_along(conceptSet))
  )
}
subsetTable <- function(x) {
  cdm <- omopgenerics::cdmReference(x)

  # domains
  x <- x |>
    dplyr::inner_join(
      cdm[["concept"]] |> dplyr::select("concept_id", "domain_id"),
      by = "concept_id"
    ) |>
    dplyr::compute()
  domains <- x |>
    dplyr::select("domain_id") |>
    dplyr::distinct() |>
    dplyr::pull() |>
    tolower() |>
    strsplit(split = "/") |>
    unlist()
  domains[domains == "obs"] <- "observation"
  domains <- unique(domains)

  lapply(domains, function(domain) {
    tableName <- switch(
      domain,
      "device" = "device_exposure",
      "specimen" = "specimen",
      "measurement" = "measurement",
      "drug" = "drug_exposure",
      "condition" = "condition_occurrence",
      "observation" = "observation",
      "procedure" = "procedure_occurrence",
      NA_character_
    )
    if (tableName %in% names(cdm)) {
      concept <- standardConceptIdColumn(tableName)
      start <- startDateColumn(tableName)
      end <- endDateColumn(tableName)
      if (is.na(end)) {
        end <- start
      }
      res <- cdm[[tableName]] |>
        dplyr::select(
          "event_start_date" = .env$start, "event_end_date" = .env$end,
          "concept_id" = .env$concept, "person_id"
        ) |>
        dplyr::inner_join(
          x |> dplyr::select("concept_id", "concept_set_id"),
          by = "concept_id"
        )
    } else {
      if (!is.na(tableName)) {
        cli::cli_alert_warning("{.pkg tableName} not found in cdm object.")
      } else {
        cli::cli_alert_warning("domain {domain} not supported.")
      }
      res <- cdm[["concept"]] |>
        dplyr::select("concept_id") |>
        dplyr::mutate(
          "event_start_date" = as.Date("2000-01-01"),
          "event_end_date" = as.Date("2000-01-01"),
          "concept_set_id" = as.integer(0),
          "person_id" = as.integer(0)
        ) |>
        utils::head(0)
    }
    return(res)
  }) |>
    purrr::reduce(dplyr::union_all)
}

#' It creates column to indicate the flag overlap information between a table
#' and a concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetStartDate Event start date to use for the intersection.
#' @param targetEndDate Event end date to use for the intersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#'  concept <- dplyr::tibble(
#'   concept_id = c(1125315),
#'   domain_id = "Drug",
#'   vocabulary_id = NA_character_,
#'   concept_class_id = "Ingredient",
#'   standard_concept = "S",
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01"),
#'   invalid_reason = NA_character_
#'  ) %>%
#'  dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#'  cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' result <- cdm$cohort1 %>%
#'  addConceptIntersectFlag(
#'   conceptSet = list("acetaminophen"=1125315)
#'   ) %>%
#'  dplyr::collect()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#'  }
#'
addConceptIntersectFlag <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetStartDate = "event_start_date",
                                    targetEndDate = "event_end_date",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = "first",
    value = "flag",
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the count overlap information between a table
#' and a concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetStartDate Event start date to use for the intersection.
#' @param targetEndDate Event end date to use for the intersection.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#'  concept <- dplyr::tibble(
#'   concept_id = c(1125315),
#'   domain_id = "Drug",
#'   vocabulary_id = NA_character_,
#'   concept_class_id = "Ingredient",
#'   standard_concept = "S",
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01"),
#'   invalid_reason = NA_character_
#'  ) %>%
#'  dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#'  cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' result <- cdm$cohort1 %>%
#'  addConceptIntersectCount(
#'   conceptSet = list("acetaminophen"=1125315)
#'   ) %>%
#'  dplyr::collect()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#'  }
#'
addConceptIntersectCount <- function(x,
                                     conceptSet,
                                     indexDate = "cohort_start_date",
                                     censorDate = NULL,
                                     window = list(c(0, Inf)),
                                     targetStartDate = "event_start_date",
                                     targetEndDate = "event_end_date",
                                     nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    order = "first",
    value = "count",
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the date overlap information between a table
#' and a concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetDate Event date to use for the intersection.
#' @param order last or first date to use for date/days calculations.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#'  concept <- dplyr::tibble(
#'   concept_id = c(1125315),
#'   domain_id = "Drug",
#'   vocabulary_id = NA_character_,
#'   concept_class_id = "Ingredient",
#'   standard_concept = "S",
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01"),
#'   invalid_reason = NA_character_
#'  ) %>%
#'  dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#'  cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' result <- cdm$cohort1 %>%
#'  addConceptIntersectDate(
#'   conceptSet = list("acetaminophen"=1125315)
#'   ) %>%
#'  dplyr::collect()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#'  }
#'
addConceptIntersectDate <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetDate = "event_start_date",
                                    order = "first",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetDate,
    targetEndDate = NULL,
    order = order,
    value = "date",
    nameStyle = nameStyle
  )
}

#' It creates column to indicate the days of difference from an index date to a
#' concept
#'
#' @param x Table with individuals in the cdm.
#' @param conceptSet Concept set list.
#' @param indexDate Variable in x that contains the date to compute the
#' intersection.
#' @param censorDate whether to censor overlap events at a date column of x
#' @param window window to consider events in.
#' @param targetDate Event date to use for the intersection.
#' @param order last or first date to use for date/days calculations.
#' @param nameStyle naming of the added column or columns, should include
#' required parameters.
#'
#' @return table with added columns with overlap information
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' cdm <- mockPatientProfiles()
#'  concept <- dplyr::tibble(
#'   concept_id = c(1125315),
#'   domain_id = "Drug",
#'   vocabulary_id = NA_character_,
#'   concept_class_id = "Ingredient",
#'   standard_concept = "S",
#'   concept_code = NA_character_,
#'   valid_start_date = as.Date("1900-01-01"),
#'   valid_end_date = as.Date("2099-01-01"),
#'   invalid_reason = NA_character_
#'  ) %>%
#'  dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
#'  cdm <- CDMConnector::insertTable(cdm, "concept", concept)
#' result <- cdm$cohort1 %>%
#'  addConceptIntersectDays(
#'   conceptSet = list("acetaminophen"=1125315)
#'   ) %>%
#'  dplyr::collect()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#'  }
#'
addConceptIntersectDays <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetDate = "event_start_date",
                                    order = "first",
                                    nameStyle = "{concept_name}_{window_name}") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetDate,
    targetEndDate = NULL,
    order = order,
    value = "days",
    nameStyle = nameStyle
  )
}
