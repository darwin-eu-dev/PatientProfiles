test_that("check input length and type for each of the arguments", {
  cdm <-
    mockPatientProfiles(connectionDetails,
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2019-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )

  expect_error(addPriorObservation("cdm$cohort1", cdm))

  expect_error(addPriorObservation(cdm$cohort1, "cdm"))

  expect_error(addPriorObservation(cdm$cohort1, cdm, indexDate = "end_date"))
})

test_that("check condition_occurrence and cohort1 work", {
  # mock data
  cdm <-
    mockPatientProfiles(connectionDetails,
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2005-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )
  # check it works with cohort1 table in mockdb
  expect_true(typeof(cdm$cohort1 %>% addPriorObservation(cdm) %>% dplyr::collect()) == "list")
  expect_true("prior_observation" %in% colnames(cdm$cohort1 %>% addPriorObservation(cdm)))
  # check it works with condition_occurrence table in mockdb
  expect_true(typeof(cdm$condition_occurrence %>% addPriorObservation(cdm, indexDate = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("prior_observation" %in% colnames(cdm$condition_occurrence %>% addPriorObservation(cdm, indexDate = "condition_start_date")))
})

test_that("check working example with cohort1", {
  # create mock tables for testing
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  obs1 <- dplyr::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-02-03"),
      as.Date("2010-02-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2014-01-01"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )

  cdm <-
    mockPatientProfiles(connectionDetails,
      seed = 1,
      cohort1 = cohort1,
      observation_period = obs1
    )

  result <- cdm$cohort1 %>%
    addPriorObservation(cdm) %>%
    dplyr::collect()

  expect_true(all(colnames(cohort1) %in% colnames(result)))

  expect_true(all(result %>% dplyr::select("prior_observation") == dplyr::tibble(prior_observation = c(28, 28, 31))))
})

test_that("check working example with condition_occurrence", {
  # create mock tables for testing
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = c("1", "1", "1"),
    person_id = c("1", "2", "3"),
    condition_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    condition_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  obs1 <- dplyr::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-02-03"),
      as.Date("2010-02-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2014-01-01"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )

  cdm <-
    mockPatientProfiles(connectionDetails,
      seed = 1,
      condition_occurrence = condition_occurrence,
      observation_period = obs1
    )

  result <-
    cdm$condition_occurrence %>%
    addPriorObservation(cdm, indexDate = "condition_start_date") %>%
    dplyr::collect()

  expect_true(all(
    result %>% dplyr::select("prior_observation") == dplyr::tibble(prior_observation = c(28, 28, 31))
  ))
})

test_that("different name", {
  # create mock tables for testing
  conditionOccurrence <- dplyr::tibble(
    condition_occurrence_id = c("1", "1", "1"),
    person_id = c("1", "2", "3"),
    condition_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    condition_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  obs1 <- dplyr::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-02-03"),
      as.Date("2010-02-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2014-01-01"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )

  cdm <-
    mockPatientProfiles(connectionDetails,
      seed = 1,
      condition_occurrence = conditionOccurrence,
      observation_period = obs1
    )

  cdm$condition_occurrence <-
    cdm$condition_occurrence %>%
    addPriorObservation(cdm,
      indexDate = "condition_start_date",
      priorObservationName = "ph"
    )
  expect_true("ph" %in% colnames(cdm$condition_occurrence))
})

test_that("priorHistory and future_observation - outside of observation period", {
  # priorHistory should be NA if index date is outside of an observation period

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2014-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2001-01-01"),
      as.Date("2015-01-01")
    )
  )
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(connectionDetails,
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addPriorObservation(cdm,
      indexDate = "cohort_start_date"
    )
  # both should be NA
  expect_true(all(is.na(cdm$cohort1a %>% dplyr::pull(prior_observation))))
})

test_that("multiple observation periods", {
  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 2),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-01-01"),
      as.Date("2015-01-01"),
      as.Date("2015-01-01")
    )
  )
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(connectionDetails,
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addPriorObservation(cdm,
      indexDate = "cohort_start_date"
    )

  expect_true(nrow(cdm$cohort1a %>% dplyr::collect()) == 2)
  expect_true(all(cdm$cohort1a %>% dplyr::pull(prior_observation) ==
    as.numeric(difftime(as.Date("2012-02-01"),
      as.Date("2010-01-01"),
      units = "days"
    ))))
})
