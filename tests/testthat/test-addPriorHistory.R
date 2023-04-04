test_that("check input length and type for each of the arguments", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2019-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )

  expect_error(addPriorHistory("cdm$cohort1", cdm))

  expect_error(addPriorHistory(cdm$cohort1, "cdm"))

  expect_error(addPriorHistory(cdm$cohort1, cdm, indexDate = "end_date"))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check condition_occurrence and cohort1 work", {

  # mock data
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2005-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )
  # check it works with cohort1 table in mockdb
  expect_true(typeof(cdm$cohort1 %>% addPriorHistory(cdm) %>% dplyr::collect()) == "list")
  expect_true("prior_history" %in% colnames(cdm$cohort1 %>% addPriorHistory(cdm)))
  # check it works with condition_occurrence table in mockdb
  expect_true(typeof(cdm$condition_occurrence %>% addPriorHistory(cdm, indexDate = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("prior_history" %in% colnames(cdm$condition_occurrence %>% addPriorHistory(cdm, indexDate = "condition_start_date")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check working example with cohort1", {
  # create mock tables for testing
  cohort1 <- tibble::tibble(
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

  obs_1 <- tibble::tibble(
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
    mockPatientProfiles(
      seed = 1,
      cohort1 = cohort1,
      observation_period = obs_1
    )

  result <- cdm$cohort1 %>%
    addPriorHistory(cdm) %>%
    dplyr::collect()

  expect_true(all(colnames(cohort1) %in% colnames(result)))

  expect_true(all(result %>% dplyr::select("prior_history") == tibble::tibble(prior_history = c(28, 28, 31))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check working example with condition_occurrence", {
  # create mock tables for testing
  condition_occurrence <- tibble::tibble(
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

  obs_1 <- tibble::tibble(
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
    mockPatientProfiles(
      seed = 1,
      condition_occurrence = condition_occurrence,
      observation_period = obs_1
    )

  result <-
    cdm$condition_occurrence %>%
    addPriorHistory(cdm, indexDate = "condition_start_date") %>%
    dplyr::collect()

  expect_true(all(
    result %>% dplyr::select("prior_history") == tibble::tibble(prior_history = c(28, 28, 31))
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("different name", {
  # create mock tables for testing
  condition_occurrence <- tibble::tibble(
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

  obs_1 <- tibble::tibble(
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
    mockPatientProfiles(
      seed = 1,
      condition_occurrence = condition_occurrence,
      observation_period = obs_1
    )

  cdm$condition_occurrence <-
    cdm$condition_occurrence %>%
    addPriorHistory(cdm,
      indexDate = "condition_start_date",
      priorHistoryName = "ph"
    )
  expect_true("ph" %in% names(cdm$condition_occurrence))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("priorHistory and future_observation - outside of observation period", {

  # priorHistory should be NA if index date is outside of an observation period

  person <- tibble::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
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
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addPriorHistory(cdm,
      indexDate = "cohort_start_date"
    )
  # both should be NA
  expect_true(all(is.na(cdm$cohort1a %>% dplyr::pull(prior_history))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("multiple observation periods", {

  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- tibble::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
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
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addPriorHistory(cdm,
      indexDate = "cohort_start_date"
    )

  expect_true(nrow(cdm$cohort1a %>% dplyr::collect()) == 2)
  expect_true(all(cdm$cohort1a %>% dplyr::pull(prior_history) ==
    as.numeric(difftime(as.Date("2012-02-01"),
      as.Date("2010-01-01"),
      units = "days"
    ))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
