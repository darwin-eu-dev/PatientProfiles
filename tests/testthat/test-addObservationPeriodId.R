test_that("add observation period id", {
  skip_on_cran()

  person <- dplyr::tibble(
    person_id = c(1L, 2L, 3L),
    gender_concept_id = 1L,
    year_of_birth = 1990L,
    race_concept_id = 1L,
    ethnicity_concept_id = 1L
  )

  observation_period <- dplyr::tibble(
    person_id = c(1L, 1L, 2L, 3L),
    observation_period_start_date = as.Date(c(
      "2010-01-01", "2019-01-01", "2019-01-01", "2019-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2016-01-01", "2021-01-01", "2022-01-01", "2019-01-01"
    )),
    observation_period_id = c(1L, 2L, 3L, 4L),
    period_type_concept_id = 0L
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    observation_period = observation_period
  )

  my_cohort <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L, 1L, 1L),
    subject_id = c(1L, 1L, 2L, 3L),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2015-05-12", "2020-01-01", "2020-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-01", "2015-05-12", "2020-01-01", "2020-01-01"
    ))
  )

  cdm <- omopgenerics::insertTable(
    cdm = cdm, name = "my_cohort", table = my_cohort
  )

  # note we have a cohort entry outside of observation to test expected NA
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort,
    .softValidation = TRUE
  )

  cdm$my_cohort_obs <- cdm$my_cohort |>
    addObservationPeriodId()
  expect_true("observation_period_id" %in% colnames(cdm$my_cohort_obs))

  expect_true(cdm$my_cohort_obs |>
    dplyr::filter(subject_id == 1) |>
    dplyr::filter(cohort_definition_id == 1L) |>
    dplyr::pull("observation_period_id") == 2L)
  expect_true(cdm$my_cohort_obs |>
    dplyr::filter(subject_id == 1) |>
    dplyr::filter(cohort_definition_id == 2L) |>
    dplyr::pull("observation_period_id") == 1L)
  expect_true(cdm$my_cohort_obs |>
    dplyr::filter(subject_id == 2) |>
    dplyr::pull("observation_period_id") == 1L)
  expect_true(is.na(cdm$my_cohort_obs |>
    dplyr::filter(subject_id == 3) |>
    dplyr::pull("observation_period_id")))

  # overwrite variable if it already exists
  expect_warning(cdm$my_cohort_obs <- cdm$my_cohort_obs |>
    addObservationPeriodId())

  # observation_period_id is overwritten
  expect_warning(x <- cdm$observation_period |>
    addObservationPeriodId(indexDate = "observation_period_end_date"))
  expect_identical(
    x |>
      dplyr::collect() |>
      dplyr::arrange(.data$person_id, .data$observation_period_start_date) |>
      dplyr::pull("observation_period_id"),
    x |>
      dplyr::group_by(.data$person_id) |>
      dplyr::arrange(.data$observation_period_start_date) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::collect() |>
      dplyr::arrange(.data$person_id, .data$observation_period_start_date) |>
      dplyr::pull("id")
  )

  # must have either person or subject id
  expect_error(cdm$drug_exposure |>
    dplyr::select(!"person_id") |>
    addObservationPeriodId(indexDate = "drug_exposure_start_date"))

  expect_error(cdm$drug_exposure |>
    dplyr::mutate("subject_id" = 1L) |>
    addObservationPeriodId(indexDate = "drug_exposure_start_date"))

  # must have index date column
  expect_error(cdm$drug_exposure |>
    addObservationPeriodId(indexDate = "cohort_start_date"))
  expect_no_error(cdm$drug_exposure |>
    addObservationPeriodId(indexDate = "drug_exposure_start_date"))

  # no error if empty
  expect_no_error(
    cdm$drug_exposure |>
      dplyr::filter(person_id == 0) |>
      addObservationPeriodId(indexDate = "drug_exposure_start_date")
  )

  # check name
  expect_warning(
    cdm$my_cohort_obs <- cdm$my_cohort_obs |>
      dplyr::select(-"observation_period_id") |>
      addObservationPeriodId(name = "my_cohort_obs")
  )
  expect_no_error(
    cdm$my_cohort_2 <- cdm$my_cohort_obs |>
      dplyr::select(-"observation_period_id") |>
      addObservationPeriodId(name = "my_cohort_2")
  )
  expect_error(
    cdm$my_cohort_2 <- cdm$my_cohort_obs |>
      dplyr::select(-"observation_period_id") |>
      addObservationPeriodId(name = "my_cohort_3")
  )

  # check nameObservationPeriodId
  expect_no_error(
    cdm$my_cohort_2 <- cdm$my_cohort_obs |>
      addObservationPeriodId(nameObservationPeriodId = "custom_name")
  )
  expect_true("custom_name" %in% colnames(cdm$my_cohort_2))
  expect_warning(
    cdm$my_cohort_2 <- cdm$my_cohort_obs |>
      addObservationPeriodId(nameObservationPeriodId = "camelCase")
  )
  expect_true("camel_case" %in% colnames(cdm$my_cohort_2))
  expect_false("camelCase" %in% colnames(cdm$my_cohort_2))

  expect_error(
    cdm$my_cohort_obs |>
      addObservationPeriodId(nameObservationPeriodId = NA_character_)
  )
  expect_error(
    cdm$my_cohort_obs |>
      addObservationPeriodId(nameObservationPeriodId = NULL)
  )
  expect_error(
    cdm$my_cohort_obs |>
      addObservationPeriodId(nameObservationPeriodId = c("id1", "id2"))
  )

  # check query
  expect_no_error(
    x <- cdm$my_cohort |>
      addObservationPeriodId() |>
      dplyr::collect()
  )
  expect_no_error(
    y <- cdm$my_cohort |>
      addObservationPeriodIdQuery() |>
      dplyr::collect()
  )
  expect_identical(x, y)

  mockDisconnect(cdm = cdm)
})
