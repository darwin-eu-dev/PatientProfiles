# Copyright 2024 DARWIN EU (C)
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

#' It creates a mock database for testing PatientProfiles package
#'
#' @param con A DBI connection to create the cdm mock object.
#' @param writeSchema Name of an schema on the same connection with writing
#' permisions.
#' @param numberIndividuals Number of individuals to create in the cdm
#' reference.
#' @param ... User self defined tables to put in cdm, it can input as many
#' as the user want.
#' @param seed A number to set the seed. If NULL seed is not used.
#'
#' @return A mock cdm_reference object created following user's specifications.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#' library(CDMConnector)
#'
#' cdm <- mockPatientProfiles()
#'
#' mockDisconnect(cdm = cdm)
#' }
#'
mockPatientProfiles <- function(con = NULL,
                                writeSchema = NULL,
                                numberIndividuals = 10,
                                ...,
                                seed = NULL) {
  if (is.null(con)) {
    checkInstalled("duckdb")
    con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  }
  if (!inherits(con, "DBIConnection")) {
    cli::cli_abort(c("!" = "`con` must be a DBI connection"))
  }
  if (is.null(writeSchema) & class(con) == "duckdb_connection") {
    writeSchema <- "main"
  }
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  # Put ... into a list
  tables <- list(...)
  assertList(tables, named = TRUE, class = "data.frame")

  # get persons
  if (length(tables) == 0) {
    persons <- seq_len(numberIndividuals)
  } else {
    persons <- numeric()
    for (k in seq_along(tables)) {
      x <- tables[[k]]
      if ("person_id" %in% colnames(x)) {
        persons <- c(persons, x[["person_id"]])
      } else if ("subject_id" %in% colnames(x)) {
        persons <- c(persons, x[["subject_id"]])
      }
    }
    persons <- unique(persons)
  }
  n <- length(persons)

  # create person table
  if (!"person" %in% names(tables)) {
    tables[["person"]] <- dplyr::tibble(
      "person_id" = persons,
      "gender_concept_id" = sample(c(8532, 8507), n, TRUE),
      "year_of_birth" = 1900L + sample.int(120, n, TRUE),
      "race_concept_id" = 0L,
      "ethnicity_concept_id" = 0L
    )
  }

  # get dates
  dates <- dplyr::tibble("person_id" = integer(), "date" = as.Date(character()))
  for (k in seq_along(tables)) {
    x <- tables[[k]]
    cols <- colnames(x)
    id <- c("person_id", "subject_id")
    id <- id[id %in% cols]
    if (length(id) == 1) {
      colDates <- cols[grepl("date", cols)]
      for (i in seq_along(colDates)) {
        dates <- dates |>
          dplyr::union_all(dplyr::tibble(
            "person_id" = x[[id]], "date" = as.Date(x[[colDates[i]]])
          ))
      }
    }
  }

  # create observation_period
  if (!"observation_period" %in% names(tables)) {
    if (nrow(dates) == 0) {
      tables[["observation_period"]] <- tables[["person"]] |>
        dplyr::select("person_id", "year_of_birth") |>
        dplyr::mutate(
          "observation_period_start_date" = as.Date(NA),
          "observation_period_end_date" = as.Date(NA)
        )
    } else {
      tables[["observation_period"]] <- tables[["person"]] |>
        dplyr::select("person_id", "year_of_birth") |>
        dplyr::left_join(
          dates |>
            dplyr::group_by(.data$person_id) |>
            dplyr::summarise(
              "observation_period_start_date" = min(.data$date, na.rm = TRUE),
              "observation_period_end_date" = max(.data$date, na.rm = TRUE)
            ),
          by = "person_id"
        )
    }
    tables[["observation_period"]] <- tables[["observation_period"]] |>
      dplyr::mutate(
        "observation_period_start_date" = dplyr::if_else(
          is.na(.data$observation_period_start_date),
          as.Date(
            x = paste0(
              .data$year_of_birth + sample.int(120, n, TRUE), "-", sample(1:12, n, TRUE), "-",
              sample(1:28, n, TRUE)
            ),
            format = "%Y-%m-%d"
          ),
          .data$observation_period_start_date
        ),
        "observation_period_end_date" = dplyr::if_else(
          is.na(.data$observation_period_end_date),
          .data$observation_period_start_date + sample.int(4e4, n, TRUE),
          .data$observation_period_end_date
        ),
        "period_type_concept_id" = 0L,
        "observation_period_id" = dplyr::row_number(),
        "observation_period_start_date" = dplyr::if_else(
          as.integer(format(.data$observation_period_start_date, "%Y")) >=
            .data$year_of_birth,
          as.Date(paste0(.data$year_of_birth, "-01-01")),
          .data$observation_period_start_date
        )
      ) |>
      dplyr::select(-"year_of_birth")
  }

  # correct person
  tables[["person"]] <- tables[["person"]] |>
    dplyr::left_join(
      tables[["observation_period"]] |>
        dplyr::group_by(.data$person_id) |>
        dplyr::summarise(
          "start_year" = min(.data$observation_period_start_date) |>
            format("%Y") |>
            as.integer(),
          .groups = "drop"
        ),
      by = "person_id"
    ) |>
    dplyr::mutate("year_of_birth" = dplyr::if_else(
      .data$year_of_birth < .data$start_year,
      .data$year_of_birth,
      .data$start_year
    )) |>
    dplyr::select(-"start_year")

  # create drug_exposure
  if (!"drug_exposure" %in% names(tables)) {
    nr <- sample.int(n * 2, 1)
    tables[["drug_exposure"]] <- dplyr::tibble(
      "person_id" = sample(tables$person$person_id, size = nr, TRUE)
    ) |>
      dplyr::inner_join(tables[["observation_period"]], by = "person_id") |>
      addDate(c("drug_exposure_start_date", "drug_exposure_end_date")) |>
      dplyr::mutate(
        "drug_exposure_id" = seq_len(nr),
        "drug_concept_id" = sample.int(10, nr, T),
        "drug_type_concept_id" = 0L
      )
  }

  # create condition_occurrence
  if (!"condition_occurrence" %in% names(tables)) {
    nr <- sample.int(n * 2, 1)
    tables[["condition_occurrence"]] <- dplyr::tibble(
      "person_id" = sample(tables$person$person_id, size = nr, TRUE)
    ) |>
      dplyr::inner_join(tables[["observation_period"]], by = "person_id") |>
      addDate(c("condition_start_date", "condition_end_date")) |>
      dplyr::mutate(
        "condition_occurrence_id" = seq_len(nr),
        "condition_concept_id" = sample.int(10, nr, T),
        "condition_type_concept_id" = 0L
      )
  }

  # create death
  if (!"death" %in% names(tables)) {
    nn <- round(n * 0.2)
    if (nn > 0) {
      nr <- sample.int(nn, 1)
    } else {
      nr <- 0
    }
    tables[["death"]] <- dplyr::tibble(
      "person_id" = sample(tables$person$person_id, size = nr, FALSE)
    ) |>
      dplyr::inner_join(tables[["observation_period"]], by = "person_id") |>
      addDate(c("death_date"))
  }

  # create cohort1
  if (!"cohort1" %in% names(tables)) {
    tables[["cohort1"]] <- dplyr::tibble(
      "person_id" = sample(tables$person$person_id)
    ) |>
      dplyr::inner_join(tables[["observation_period"]], by = "person_id") |>
      addDate(c("cohort_start_date", "cohort_end_date")) |>
      dplyr::mutate("cohort_definition_id" = sample.int(3, n, T)) |>
      dplyr::rename("subject_id" = "person_id")
  }

  # create cohort2
  if (!"cohort2" %in% names(tables)) {
    tables[["cohort2"]] <- dplyr::tibble(
      "person_id" = sample(tables$person$person_id)
    ) |>
      dplyr::inner_join(tables[["observation_period"]], by = "person_id") |>
      addDate(c("cohort_start_date", "cohort_end_date")) |>
      dplyr::mutate("cohort_definition_id" = sample.int(3, n, T)) |>
      dplyr::rename("subject_id" = "person_id")
  }

  # into database
  tablesToInsert <- names(tables)
  src <- CDMConnector::dbSource(con = con, writeSchema = writeSchema)

  for (tab in names(tables)) {
    omopgenerics::insertTable(
      cdm = src, name = tab, table = tables[[tab]], overwrite = TRUE
    ) |>
      invisible()
  }

  # create the cdm object
  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = writeSchema,
    write_schema = writeSchema,
    cohort_tables = names(tables)[
      !names(tables) %in% omopgenerics::omopTables()
    ],
    cdm_name = "PP_MOCK"
  )

  return(cdm)
}

checkInstalled <- function(name, call = parent.frame()) {
  pkgs <- .packages(all.available = TRUE)
  notInstalled <- name[!name %in% pkgs]
  if (length(notInstalled) > 0) {
    cli::cli_abort("{.pkg {notInstalled}} {?is/are} not installed.")
  }
  return(invisible(NULL))
}
addDate <- function(x, cols) {
  if (nrow(x) == 0) {
    x <- x |> dplyr::select("person_id")
    for (col in cols) {
      x <- x |> dplyr::mutate(!!col := as.Date(character()))
    }
    return(x)
  }
  x <- x |>
    dplyr::select(
      "person_id",
      "observation_period_start_date",
      "observation_period_end_date"
    ) |>
    dplyr::rowwise()
  for (col in cols) {
    x <- x |>
      dplyr::mutate(
        "diff" = as.integer(difftime(
          .data$observation_period_end_date,
          .data$observation_period_start_date
         )),
        "days" = sample.int(.data$diff + 1, 1) - 1 ,
        !!col := .data$observation_period_start_date + .data$days,
        "observation_period_start_date" = .data[[col]]
      )
  }
  x <- x |>
    dplyr::ungroup() |>
    dplyr::select(
      -"observation_period_start_date", -"observation_period_end_date", -"diff",
      -"days"
    )
  return(x)
}

#' Function to disconnect from the mock
#'
#' @param cdm A cdm_reference object.
#'
#' @export
#'
mockDisconnect <- function(cdm) {
  cdm <- omopgenerics::dropTable(cdm = cdm, name = dplyr::everything())
  if ("db_cdm" %in% class(omopgenerics::cdmSource(cdm = cdm))) {
    con <- CDMConnector::cdmCon(cdm = cdm)
    DBI::dbDisconnect(conn = con, shutdown = TRUE)
  }
  return(invisible(NULL))
}
