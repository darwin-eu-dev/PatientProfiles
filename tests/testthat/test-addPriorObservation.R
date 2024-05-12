test_that("check input length and type for each of the arguments", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  expect_error(addPriorObservation("cdm$cohort1"))

  expect_error(addPriorObservation(cdm$cohort1, indexDate = "end_date"))

  mockDisconnect(cdm = cdm)
})

test_that("check condition_occurrence and cohort1 work", {
  skip_on_cran()
  # mock data
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  # check it works with cohort1 table in mockdb
  expect_true(typeof(cdm$cohort1 %>% addPriorObservation() %>% dplyr::collect()) == "list")
  expect_true("prior_observation" %in% colnames(cdm$cohort1 %>% addPriorObservation()))
  # check it works with condition_occurrence table in mockdb
  expect_true(typeof(cdm$condition_occurrence %>% addPriorObservation(indexDate = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("prior_observation" %in% colnames(cdm$condition_occurrence %>% addPriorObservation(indexDate = "condition_start_date")))

  mockDisconnect(cdm = cdm)
})

test_that("check working example with cohort1", {
  skip_on_cran()
  # create mock tables for testing
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1:3,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2010-03-03", "2010-03-01", "2010-02-01")),
    cohort_end_date = as.Date(c("2011-01-01", "2011-01-01", "2011-01-01"))
  )

  obs1 <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "2010-02-03", "2010-02-01", "2010-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2014-01-01", "2012-01-01", "2012-01-01"
    )),
    period_type_concept_id = 0L
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 1,
    cohort1 = cohort1,
    observation_period = obs1,
    cohort2 = cohort1
  )

  result <- cdm$cohort1 %>%
    addPriorObservation() %>%
    dplyr::collect()

  expect_true(all(colnames(cohort1) %in% colnames(result)))

  expect_true(all(
    result |>
      dplyr::arrange(.data$subject_id) |>
      dplyr::pull("prior_observation") == c(28, 28, 31)
  ))

  mockDisconnect(cdm = cdm)
})

test_that("check working example with condition_occurrence", {
  skip_on_cran()
  # create mock tables for testing
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1L,
    person_id = 1:3,
    condition_start_date = as.Date(c("2010-03-03", "2010-03-01", "2010-02-01")),
    condition_end_date = as.Date(c("2015-01-01", "2013-01-01", "2013-01-01")),
    condition_concept_id = 0L,
    condition_type_concept_id = 0L
  )

  obs1 <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date(c(
      "2010-02-03", "2010-02-01", "2010-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2014-01-01", "2012-01-01", "2012-01-01"
    )),
    period_type_concept_id = 0L
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 1,
    condition_occurrence = condition_occurrence,
    observation_period = obs1
  )

  result <- cdm$condition_occurrence %>%
    addPriorObservation(indexDate = "condition_start_date") %>%
    dplyr::collect()

  expect_true(all(
    result |>
      dplyr::arrange(.data$person_id) |>
      dplyr::pull("prior_observation") == c(28, 28, 31)
  ))

  mockDisconnect(cdm = cdm)
})

test_that("different name", {
  skip_on_cran()
  # create mock tables for testing
  conditionOccurrence <- dplyr::tibble(
    condition_occurrence_id = 1L,
    person_id = 1:3,
    condition_start_date = as.Date(c("2010-03-03", "2010-03-01", "2010-02-01")),
    condition_end_date = as.Date(c("2015-01-01", "2013-01-01", "2013-01-01")),
    condition_concept_id = 0L,
    condition_type_concept_id = 0L
  )

  obs1 <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date(c(
      "2010-02-03", "2010-02-01", "2010-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2014-01-01", "2012-01-01", "2012-01-01"
    )),
    period_type_concept_id = 0L
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 1,
    condition_occurrence = conditionOccurrence,
    observation_period = obs1
  )

  cdm$condition_occurrence <- cdm$condition_occurrence %>%
    addPriorObservation(
      indexDate = "condition_start_date", priorObservationName = "ph"
    )
  expect_true("ph" %in% colnames(cdm$condition_occurrence))

  x <- cdm$cohort1 |>
    addPriorObservation(priorObservationType = "days") |>
    addPriorObservation(
      priorObservationType = "date", priorObservationName = "col"
    ) |>
    dplyr::left_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" = "person_id",
          "obs_start" = "observation_period_start_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::mutate("diff" = !!CDMConnector::datediff(
      "cohort_start_date", "obs_start"
    )) |>
    dplyr::collect()

  expect_equal(x$prior_observation, -x$diff)
  expect_equal(x$col, x$obs_start)

  mockDisconnect(cdm = cdm)
})

test_that("multiple observation periods", {
  skip_on_cran()
  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1L,
    year_of_birth = 1980L,
    month_of_birth = 1L,
    day_of_birth = 1L,
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 2),
    observation_period_start_date = as.Date(c(
      "2000-01-01", "2010-01-01", "2010-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2005-01-01", "2015-01-01", "2015-01-01"
    )),
    period_type_concept_id = 0L
  )
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addPriorObservation(indexDate = "cohort_start_date")

  expect_true(nrow(cdm$cohort1a %>% dplyr::collect()) == 2)
  expect_true(all(
    cdm$cohort1a %>%
      dplyr::pull(prior_observation) == as.numeric(difftime(
        as.Date("2012-02-01"), as.Date("2010-01-01"), units = "days"
      ))
  ))

  mockDisconnect(cdm = cdm)
})
