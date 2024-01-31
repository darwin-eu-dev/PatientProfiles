test_that("basic functionality summarise large scale characteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = c(8507, 8532),
    year_of_birth = c(1990, 1992),
    month_of_birth = c(1, 1),
    day_of_birth = c(1, 1),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2011-10-07", "2000-01-01")),
    observation_period_end_date = as.Date(c("2031-10-07", "2030-01-01")),
    period_type_concept_id = 44814724
  )
  cohort_interest <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    ))
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:11,
    person_id = c(rep(1, 8), rep(2, 3)),
    drug_concept_id = c(
      rep(1125315, 2), rep(1503328, 5), 1516978, 1125315, 1503328, 1516978
    ),
    drug_exposure_start_date = as.Date(c(
      "2010-10-01", "2012-12-31", "2010-01-01", "2012-09-01", "2013-04-01",
      "2014-10-31", "2015-05-01", "2015-10-01", "2012-01-01", "2012-10-01",
      "2014-10-12"
    )),
    drug_exposure_end_date = as.Date(c(
      "2010-12-01", "2013-05-12", "2011-01-01", "2012-10-01", "2013-05-01",
      "2014-12-31", "2015-05-02", "2016-10-01", "2012-01-01", "2012-10-30",
      "2015-01-10"
    )),
    drug_type_concept_id = 38000177,
    quantity = 1
  )
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:8,
    person_id = c(rep(1, 4), rep(2, 4)),
    condition_concept_id = c(
      317009, 378253, 378253, 4266367, 317009, 317009, 378253, 4266367
    ),
    condition_start_date = as.Date(c(
      "2012-10-01", "2012-01-01", "2014-01-01", "2010-01-01", "2015-02-01",
      "2012-01-01", "2013-10-01", "2014-10-10"
    )),
    condition_end_date = as.Date(c(
      "2013-01-01", "2012-04-01", "2014-10-12", "2015-01-01", "2015-03-01",
      "2012-04-01", "2013-12-01", NA
    )),
    condition_type_concept_id = 32020
  )
  cdm <- mockPatientProfiles(
    connectionDetails,
    person = person, observation_period = observation_period,
    cohort_interest = cohort_interest, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence
  )
  concept <- dplyr::tibble(
    concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
    domain_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_class_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01")
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
  name <- CDMConnector::inSchema(
    schema = connectionDetails$write_schema, table = "concept"
  )
  DBI::dbWriteTable(
    conn = connectionDetails$con, name = name, value = concept, overwrite = TRUE
  )
  cdm$concept <- dplyr::tbl(connectionDetails$con, name)

  expect_no_error(
    result <- cdm$cohort_interest %>%
      summariseLargeScaleCharacteristics(
        eventInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0
      )
  )
  result <- result |> visOmopResults::splitAdditional()
  conceptId <- c(317009, 317009, 378253, 378253, 4266367, 4266367)
  windowName <- rep(c("0 to 0", "-inf to -366"), 3)
  cohortName <- rep(c("cohort_1"), 6)
  count <- c(NA, 2, NA, 1, NA, 2)
  den <- c(3, 3, 3, 3, 3, 3)
  percentage <- as.character(100 * count / den)
  for (k in seq_along(conceptId)) {
    r <- result %>%
      dplyr::filter(
        .data$concept == .env$conceptId[k] &
          .data$variable_level == .env$windowName[k] &
          .data$group_level == .env$cohortName[k]
      )
    if (is.na(count[k])) {
      expect_true(nrow(r) == 0)
    } else {
      expect_true(nrow(r) == 2)
      expect_true(r$estimate_value[r$estimate_name == "count"] == count[k])
      expect_true(r$estimate_value[r$estimate_name == "percentage"] == percentage[k])
    }
  }

  expect_no_error(
    result <- cdm$cohort_interest %>%
      summariseLargeScaleCharacteristics(
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0
      )
  )
  result <- result |> visOmopResults::splitAdditional()
  conceptId <- c(317009, 317009, 378253, 378253, 4266367, 4266367)
  windowName <- rep(c("0 to 0", "-inf to -366"), 3)
  cohortName <- rep(c("cohort_1"), 6)
  count <- c(1, 2, 1, 1, 2, 2)
  den <- c(3, 3, 3, 3, 3, 3)
  percentage <- as.character(100 * count / den)
  for (k in seq_along(conceptId)) {
    r <- result %>%
      dplyr::filter(
        .data$concept == .env$conceptId[k] &
          .data$variable_level == .env$windowName[k] &
          .data$group_level == .env$cohortName[k]
      )
    if (is.na(count[k])) {
      expect_true(nrow(r) == 0)
    } else {
      expect_true(nrow(r) == 2)
      expect_true(r$estimate_value[r$estimate_name == "count"] == count[k])
      expect_true(r$estimate_value[r$estimate_name == "percentage"] == percentage[k])
    }
  }

  expect_no_error(
    result <- cdm$cohort_interest %>%
      PatientProfiles::addDemographics(
        ageGroup = list(c(0, 24), c(25, 150))
      ) %>%
      summariseLargeScaleCharacteristics(
        cdm = cdm,
        strata = list("age" = "age_group", "age & sex" = c("age_group", "sex")),
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0
      )
  )
  expect_true(all(c("cohort_1", "cohort_2") %in% result$group_level))
  expect_true(all(c("overall", "age_group", "age_group and sex") %in% result$strata_name))
  expect_true(all(c(
    "overall", "0 to 24", "25 to 150", "0 to 24 and Female",
    "25 to 150 and Male", "0 to 24 and Male"
  ) %in% result$strata_level))
  result <- result %>%
    dplyr::filter(strata_level == "0 to 24 and Female")
  result <- result |> visOmopResults::splitAdditional()
  conceptId <- c(317009, 317009, 378253, 378253, 4266367, 4266367)
  windowName <- rep(c("0 to 0", "-inf to -366"), 3)
  cohortName <- rep(c("cohort_1"), 6)
  count <- c(NA, 1, 1, NA, NA, NA)
  den <- c(1, 1, 1, 1, 1, 1)
  percentage <- as.character(100 * count / den)
  for (k in seq_along(conceptId)) {
    r <- result %>%
      dplyr::filter(
        .data$concept == .env$conceptId[k] &
          .data$variable_level == .env$windowName[k] &
          .data$group_level == .env$cohortName[k]
      )
    if (is.na(count[k])) {
      expect_true(nrow(r) == 0)
    } else {
      expect_true(nrow(r) == 2)
      expect_true(r$estimate_value[r$estimate_name == "count"] == count[k])
      expect_true(r$estimate_value[r$estimate_name == "percentage"] == percentage[k])
    }
  }

  expect_equal(class(result), c(
    "summarised_large_scale_characteristics", "summarised_result",
    "omop_result", "tbl_df", "tbl", "data.frame"
  ))

  expect_no_error(
    result <- cdm$cohort_interest %>%
      summariseLargeScaleCharacteristics(
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0, excludedCodes = 317009
      )
  )
  expect_false(any(grepl("317009", result$variable_name)))

})

test_that("basic functionality add large scale characteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = c(8507, 8532),
    year_of_birth = c(1990, 1992),
    month_of_birth = c(1, 1),
    day_of_birth = c(1, 1),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2011-10-07", "2000-01-01")),
    observation_period_end_date = as.Date(c("2031-10-07", "2030-01-01")),
    period_type_concept_id = 44814724
  )
  cohort_interest <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    ))
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:11,
    person_id = c(rep(1, 8), rep(2, 3)),
    drug_concept_id = c(
      rep(1125315, 2), rep(1503328, 5), 1516978, 1125315, 1503328, 1516978
    ),
    drug_exposure_start_date = as.Date(c(
      "2010-10-01", "2012-12-31", "2010-01-01", "2012-09-01", "2013-04-01",
      "2014-10-31", "2015-05-01", "2015-10-01", "2012-01-01", "2012-10-01",
      "2014-10-12"
    )),
    drug_exposure_end_date = as.Date(c(
      "2010-12-01", "2013-05-12", "2011-01-01", "2012-10-01", "2013-05-01",
      "2014-12-31", "2015-05-02", "2016-10-01", "2012-01-01", "2012-10-30",
      "2015-01-10"
    )),
    drug_type_concept_id = 38000177,
    quantity = 1
  )
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:8,
    person_id = c(rep(1, 4), rep(2, 4)),
    condition_concept_id = c(
      317009, 378253, 378253, 4266367, 317009, 317009, 378253, 4266367
    ),
    condition_start_date = as.Date(c(
      "2012-10-01", "2012-01-01", "2014-01-01", "2010-01-01", "2015-02-01",
      "2012-01-01", "2013-10-01", "2014-10-10"
    )),
    condition_end_date = as.Date(c(
      "2013-01-01", "2012-04-01", "2014-10-12", "2015-01-01", "2015-03-01",
      "2012-04-01", "2013-12-01", NA
    )),
    condition_type_concept_id = 32020
  )
  cdm <- mockPatientProfiles(
    connectionDetails,
    person = person, observation_period = observation_period,
    cohort_interest = cohort_interest, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence
  )
  concept <- dplyr::tibble(
    concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
    domain_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_class_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01")
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
  name <- CDMConnector::inSchema(
    schema = connectionDetails$write_schema, table = "concept"
  )
  DBI::dbWriteTable(
    conn = connectionDetails$con, name = name, value = concept, overwrite = TRUE
  )
  cdm$concept <- dplyr::tbl(connectionDetails$con, name)

  expect_no_error(
    result <- cdm$cohort_interest %>%
      dplyr::filter(.data$cohort_definition_id == 1) %>%
      addLargeScaleCharacteristics(
        window = list(c(0, 0), c(-Inf, -366)),
        eventInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0
      )
  )
  conceptId <- c(317009, 378253, 4266367)
  windowName <- "minf_to_m366"
  count <- c(2, 1, 2)
  expect_false(any(grepl("0_to_0", colnames(result))))
  for (k in seq_along(conceptId)) {
    column <- paste0("lsc_", windowName, "_", conceptId[k])
    expect_true(column %in% colnames(result))
    expect_equal(
      result %>% dplyr::pull(dplyr::all_of(column)) %>% sum(),
      count[k]
    )
  }

  expect_no_error(
    result <- cdm$cohort_interest %>%
      dplyr::filter(.data$cohort_definition_id == 1) %>%
      addLargeScaleCharacteristics(
        window = list(c(0, 0), c(-Inf, -366)),
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0
      )
  )
  conceptId <- c(317009, 317009, 378253, 378253, 4266367, 4266367)
  windowName <- rep(c("0_to_0", "minf_to_m366"), 3)
  count <- c(1, 2, 1, 1, 2, 2)
  cols <- paste0("lsc_", windowName, "_", conceptId)
  for (k in seq_along(conceptId)) {
    expect_true(cols[k] %in% colnames(result))
    expect_equal(
      result %>% dplyr::pull(dplyr::all_of(cols[k])) %>% sum(),
      count[k]
    )
  }

  expect_no_error(
    result2 <- cdm$cohort_interest %>%
      dplyr::filter(.data$cohort_definition_id == 1) %>%
      addLargeScaleCharacteristics(
        window = list(c(0, 0), c(-Inf, -366)),
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0.25
      )
  )
  expect_identical(
    result %>%
      dplyr::select(dplyr::all_of(sort(colnames(result)))) %>%
      dplyr::collect() %>%
      dplyr::arrange(.data$cohort_start_date),
    result2 %>%
      dplyr::select(dplyr::all_of(sort(colnames(result2)))) %>%
      dplyr::collect() %>%
      dplyr::arrange(.data$cohort_start_date)
  )

  expect_no_error(
    result3 <- cdm$cohort_interest %>%
      dplyr::filter(.data$cohort_definition_id == 1) %>%
      addLargeScaleCharacteristics(
        window = list(c(0, 0), c(-Inf, -366)),
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0.4
      )
  )
  presentCols <- cols[count > 1]
  expect_true(all(presentCols %in% colnames(result3)))
  noCols <- cols[count <= 1]
  expect_false(any(noCols %in% colnames(result3)))

  expect_no_error(
    result4 <- cdm$cohort_interest %>%
      dplyr::filter(.data$cohort_definition_id == 1) %>%
      addLargeScaleCharacteristics(
        window = list(c(0, 0), c(-Inf, -366)),
        episodeInWindow = c("condition_occurrence", "drug_exposure"),
        minimumFrequency = 0.75
      )
  )
  expect_false(any(cols %in% colnames(result4)))
})

