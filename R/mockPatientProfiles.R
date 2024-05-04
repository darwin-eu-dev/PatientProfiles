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
#' @param connectionDetails Connection an details to create the cdm mock object.
#' @param numberIndividuals Number of individuals to create in the cdm
#' reference.
#' @param ... User self defined tables to put in cdm, it can input as many
#' as the user want.
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
#' CDMConnector::cdmDisconnect(cdm = cdm)
#' }
#'
mockPatientProfiles <- function(con = NULL,
                                writeSchema = NULL,
                                numberIndividuals = 10,
                                ...,
                                seed = 1) {
  if (is.null(con)) {
    checkInstalled("duckdb")
    con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")

  }
  if (!is.null(con) && !inherits(x, "DBIConnection")) {
    cli::cli_abort(c("!" = "`con` must be a DBI connection"))
  }

  # Put ... into a list
  listTables <- list(...)


  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(drug_exposure_size, lower = 1)
  checkmate::assert_int(patient_size, lower = 1)
  checkmate::assert_int(drug_concept_id_size, lower = 1)
  checkmate::assert_int(ancestor_concept_id_size, lower = 1)
  checkmate::assert_int(ingredient_concept_id_size, lower = 1)
  checkmate::assertTRUE(drug_exposure_size >= patient_size)
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(drug_exposure, null.ok = TRUE)
  checkmate::assert_tibble(condition_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(visit_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(drug_strength, null.ok = TRUE)
  checkmate::assert_int(seed, lower = 1)
  checkmate::assertDate(as.Date(earliest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_condition_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_condition_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_visit_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_visit_start_date), null.ok = TRUE)
  checkmate::assert_int(min_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_days_to_condition_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_condition_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_days_to_visit_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_visit_end, lower = 1, null.ok = TRUE)
  if (!is.null(latest_date_of_birth) &
    !is.null(earliest_date_of_birth)) {
    checkmate::assertTRUE(latest_date_of_birth >= earliest_date_of_birth)
  }
  if (!is.null(earliest_observation_start_date) &
    !is.null(latest_observation_start_date)) {
    checkmate::assertTRUE(latest_observation_start_date >= earliest_observation_start_date)
  }
  if (!is.null(min_days_to_observation_end) &
    !is.null(max_days_to_observation_end)) {
    checkmate::assertTRUE(max_days_to_observation_end >= min_days_to_observation_end)
  }
  if (!is.null(earliest_condition_start_date) &
    !is.null(latest_condition_start_date)) {
    checkmate::assertTRUE(latest_condition_start_date >= earliest_condition_start_date)
  }
  if (!is.null(min_days_to_condition_end) &
    !is.null(max_days_to_condition_end)) {
    checkmate::assertTRUE(max_days_to_condition_end >= min_days_to_condition_end)
  }
  if (!is.null(earliest_visit_start_date) &
    !is.null(latest_visit_start_date)) {
    checkmate::assertTRUE(latest_visit_start_date >= earliest_visit_start_date)
  }
  if (!is.null(min_days_to_visit_end) &
    !is.null(max_days_to_visit_end)) {
    checkmate::assertTRUE(max_days_to_visit_end >= min_days_to_visit_end)
  }
  if (length(listTables) > 1) {
    checkmate::assertTRUE(length(listTables) == length(names(listTables)))
  }
  if (length(listTables) > 1) {
    for (i in length(listTables) > 1) {
      checkmate::assert_tibble(listTables[[i]], null.ok = TRUE)
    }
  }
  checkmate::reportAssertions(collection = errorMessage)



  set.seed(seed) # set seeds

  # create drug strength table
  if (is.null(drug_strength)) {
    drug_concept_id <-
      seq(1:drug_concept_id_size) # create unique drug concept id
    ingredient_concept_id <-
      seq(1:ingredient_concept_id_size) # create ingredient concept id
    amount_value <-
      c(
        rep(NA, each = ingredient_concept_id_size),
        # ingredient have missing amount value
        sample(c("10", "20", "30"),
          drug_concept_id_size - 1,
          replace = TRUE
        )
      ) # compute amount value
    amount_unit_concept_id <-
      sample(c("8576"),
        drug_concept_id_size,
        replace = TRUE
      ) #  compute unit id


    drug_strength <-
      data.frame(
        drug_concept_id = as.numeric(drug_concept_id),
        ingredient_concept_id = as.numeric(
          sample(ingredient_concept_id, drug_concept_id_size, replace = TRUE)
        ),
        amount_value = as.numeric(amount_value),
        amount_unit_concept_id = as.numeric(amount_unit_concept_id),
        # numerator_value = numeric(),
        # numerator_unit_concept_id = numeric(),
        # denominator_value = numeric(),
        # denominator_unit_concept_id = numeric(),
        # box_size = numeric(),
        valid_start_date = rep(as.Date("1900-01-01"), drug_concept_id_size),
        valid_end_date = rep(as.Date("2030-01-01"), drug_concept_id_size)
        # invalid_reason = character()
      )
  }





  # drug_exposure
  if (is.null(drug_exposure)) {
    drug_exposure_id <-
      as.integer(seq(1:drug_exposure_size)) # generate number of unique drug_exposure_id
    person_id <-
      as.integer(sample(seq(1:patient_size),
        drug_exposure_size,
        replace = TRUE
      )) # generate number of unique patient id
    drug_concept_id <-
      as.integer(sample(
        drug_strength$drug_concept_id,
        drug_exposure_size,
        replace = TRUE
      )) # assign drug concept id to to each drug exposure

    # generate drug exposure start date
    drug_exposure_start_date <-
      sample(
        seq(
          as.Date(min_drug_exposure_start_date),
          as.Date(max_drug_exposure_start_date),
          by = "day"
        ),
        drug_exposure_size,
        replace = TRUE
      )
    # generate drug exposure end date to happens after drug exposure start date
    drug_exposure_end_date <-
      drug_exposure_start_date + lubridate::days(sample(c(0, 7, 14, 21, 28, 30, 60, 90),
        drug_exposure_size,
        replace = TRUE
      ))

    days_supply <-
      as.integer(difftime(drug_exposure_end_date, drug_exposure_start_date, units = "days"))

    quantity <- days_supply + 1



    # putting into drug_exposure table
    drug_exposure <-
      data.frame(
        drug_exposure_id = as.numeric(drug_exposure_id),
        person_id = as.numeric(person_id),
        drug_concept_id = as.numeric(drug_concept_id),
        drug_exposure_start_date = drug_exposure_start_date,
        drug_exposure_end_date = drug_exposure_end_date,
        quantity = as.numeric(quantity),
        drug_type_concept_id = 0
        ##  days_supply = as.numeric(days_supply)
      )
  }
  # person table
  id <- seq(1:patient_size)
  # person gender
  gender_id <- sample(c("8507", "8532"),
    patient_size,
    replace = TRUE
  )

  if (is.null(person) | is.null(observation_period)) {
    # Define earliest possible date of birth for person table
    if (is.null(earliest_date_of_birth)) {
      earliest_date_of_birth <- as.Date("1920-01-01")
    }
    # Define latest possible date of birth for person table
    if (is.null(latest_date_of_birth)) {
      latest_date_of_birth <- as.Date("2000-01-01")
    }

    DOB <- sample(
      seq(
        as.Date(earliest_date_of_birth),
        as.Date(latest_date_of_birth),
        by = "day"
      ),
      patient_size,
      replace = TRUE
    )
    # year, month, day
    DOB_year <- as.numeric(format(DOB, "%Y"))
    DOB_month <- as.numeric(format(DOB, "%m"))
    DOB_day <- as.numeric(format(DOB, "%d"))

    # observation_period table
    # create a list of observational_period_id

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliest_observation_start_date)) {
      earliest_observation_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_observation_start_date)) {
      latest_observation_start_date <- as.Date("2010-01-01")
    }
    obs_start_date <-
      sample(
        seq(
          as.Date(earliest_observation_start_date),
          as.Date(latest_observation_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_observation_end)) {
      min_days_to_observation_end <- 5000
    }
    if (is.null(max_days_to_observation_end)) {
      max_days_to_observation_end <- 50000
    }

    obs_end_date <-
      obs_start_date + lubridate::days(
        sample(
          min_days_to_observation_end:max_days_to_observation_end,
          patient_size,
          replace = TRUE
        )
      )
  }



  if (is.null(person) | is.null(condition_occurrence)) {
    # define earliest and latest condition start date for obs table
    # if not specified by user
    if (is.null(earliest_condition_start_date)) {
      earliest_condition_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_condition_start_date)) {
      latest_condition_start_date <- as.Date("2020-01-01")
    }
    condition_start_date <-
      sample(
        seq(
          as.Date(earliest_condition_start_date),
          as.Date(latest_condition_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_condition_end)) {
      min_days_to_condition_end <- 1
    }
    if (is.null(max_days_to_condition_end)) {
      max_days_to_condition_end <- 1000
    }

    condition_end_date <-
      condition_start_date + lubridate::days(
        sample(
          min_days_to_condition_end:max_days_to_condition_end,
          patient_size,
          replace = TRUE
        )
      )

    c_concept_id <-
      seq(1:condition_concept_id_size)
    condition_concept_id <- sample(c_concept_id,
      patient_size,
      replace = TRUE
    )
  }


  if (is.null(person) | is.null(visit_occurrence)) {
    # define earliest and latest visit start date for obs table
    # if not specified by user
    if (is.null(earliest_visit_start_date)) {
      earliest_visit_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_visit_start_date)) {
      latest_visit_start_date <- as.Date("2020-01-01")
    }
    visit_start_date <-
      sample(
        seq(
          as.Date(earliest_visit_start_date),
          as.Date(latest_visit_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to visit end
    if (is.null(min_days_to_visit_end)) {
      min_days_to_visit_end <- 1
    }
    if (is.null(max_days_to_visit_end)) {
      max_days_to_visit_end <- 1000
    }

    visit_end_date <-
      visit_start_date + lubridate::days(
        sample(
          min_days_to_visit_end:max_days_to_visit_end,
          patient_size,
          replace = TRUE
        )
      )

    v_concept_id <- seq(1:visit_concept_id_size)

    visit_concept_id <- sample(v_concept_id,
      patient_size,
      replace = TRUE
    )

    v_occurrence_id <- seq(1:visit_occurrence_id_size)

    visit_occurrence_id <- sample(v_occurrence_id,
      patient_size,
      replace = TRUE
    )
  }

  if (is.null(person)) {
    person <- dplyr::tibble(
      person_id = id,
      gender_concept_id = gender_id,
      year_of_birth = DOB_year,
      month_of_birth = DOB_month,
      day_of_birth = DOB_day,
      race_concept_id = 0,
      ethnicity_concept_id = 0
    )
  }

  if (is.null(observation_period)) {
    observation_period <- dplyr::tibble(
      observation_period_id = id,
      person_id = id,
      observation_period_start_date = obs_start_date,
      observation_period_end_date = obs_end_date,
      period_type_concept_id = 0
    )
  }



  if (is.null(condition_occurrence)) {
    condition_occurrence <- dplyr::tibble(
      condition_occurrence_id = id,
      person_id = id,
      condition_concept_id = condition_concept_id,
      condition_start_date = condition_start_date,
      condition_end_date = condition_end_date,
      condition_type_concept_id = 0
    )
  }


  if (is.null(visit_occurrence)) {
    id <- sample(seq(1:patient_size))

    visit_occurrence <- dplyr::tibble(
      visit_occurrence_id = visit_occurrence_id,
      person_id = id,
      visit_concept_id = visit_concept_id,
      visit_start_date = visit_start_date,
      visit_end_date = visit_end_date,
      visit_type_concept_id = 0
    )
  }

  if (is.null(concept_ancestor)) {
    ancestor_concept_id <-
      seq(1:ancestor_concept_id_size)
    descendant_concept_id <-
      seq((ancestor_concept_id_size + 1):(ancestor_concept_id_size + ancestor_concept_id_size))
    concept_ancestor <- data.frame(
      ancestor_concept_id = as.numeric(ancestor_concept_id),
      descendant_concept_id = as.numeric(descendant_concept_id),
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  }

  # death table
  if (is.null(death)) {
    death <- dplyr::tibble(person_id = c(1),
                           death_date = as.Date("2020-04-01"))
  }

  # cohort table 1
  if (is.null(cohort1)) {
    cohort1 <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 3),
      cohort_start_date = as.Date(c("2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01")),
      cohort_end_date = as.Date(c("2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"))
    ) |>
      dplyr::filter(.data$subject_id %in% seq(1, patient_size))
  }



  # cohort table 2
  if (is.null(cohort2)) {
    cohort2 <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 3, 1),
      subject_id = c(1, 3, 1, 2, 1),
      cohort_start_date = as.Date(c("2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25")),
      cohort_end_date = as.Date(c("2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25"))
    ) |>
      dplyr::filter(.data$subject_id %in% seq(1, patient_size))
  }

  # into database
  db <- connectionDetails[["con"]]
  writeSchema <- c(
    "schema" = strsplit(connectionDetails[["write_schema"]], "\\.")[[1]],
    "prefix" = connectionDetails[["mock_prefix"]]
  )

  tablesToInsert <- c(
    "drug_strength", "drug_exposure", "person", "observation_period",
    "condition_occurrence", "visit_occurrence", "concept_ancestor", "death"
  )

  src <- CDMConnector::dbSource(
    con = connectionDetails$con, writeSchema = connectionDetails$write_schema
  )

  for (tab in tablesToInsert) {
    x <- omopgenerics::insertTable(
      cdm = src, name = tab, table = get(tab), overwrite = TRUE
    )
  }

  # create the cdm object
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = writeSchema,
    write_schema = writeSchema,
    cdm_name = "PP_MOCK"
  )

  listTables[["cohort1"]] <- cohort1
  listTables[["cohort2"]] <- cohort2
  cohorts <- names(listTables)
  for (cohort in cohorts) {
    x <- addCohortCountAttr(listTables[[cohort]])
    cdm <- omopgenerics::insertTable(
      cdm = cdm, name = cohort, table = listTables[[cohort]], overwrite = TRUE
    )
    cdm[[cohort]] <- cdm[[cohort]] |>
      omopgenerics::newCohortTable()
  }

  return(cdm)
}


#' Function to add attributes to cohort table
#' it adds cohort_count, cohort_set, cohort_count, cohort_attrition
#'
#' @noRd
#'
addCohortCountAttr <- function(cohort) {
  # add cohort set
  if (!("cohort_set" %in% attributes(cohort))) {
    attr(cohort, "cohort_set") <- cohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::mutate("cohort_name" = paste0(
        "cohort_",
        .data$cohort_definition_id
      ))
  }

  # add cohort attrition
  if (!("cohort_attrition" %in% attributes(cohort))) {
    attr(cohort, "cohort_attrition") <- cohort %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(
        number_records = dplyr::n(),
        number_subjects = dplyr::n_distinct(.data$subject_id)
      ) %>%
      dplyr::collect() %>%
      dplyr::mutate(
        "reason" = "Qualifying initial records",
        "reason_id" = 1,
        "excluded_records" = 0,
        "excluded_subjects" = 0
      )
  }

  return(cohort)
}

checkInstalled <- function(name, call = parent.frame()) {
  pkgs <- .packages(all.available = TRUE)
  notInstalled <- name[!name %in% pkgs]
  if (length(notInstalled) > 0) {
    cli::cli_abort("{.pkg {notInstalled}} {?is/are} not installed.")
  }
  return(invisible(NULL))
}

#' Function to dissconnect from the mock
