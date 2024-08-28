test_that("add observation period id", {
  skip_on_cran()
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema())

  person <- dplyr::tibble(
    person_id = c(1L,2L,3L),
    gender_concept_id = 1L,
    year_of_birth = 1990L,
    race_concept_id = 1L,
    ethnicity_concept_id = 1L
  )
  cdm <- omopgenerics::insertTable(cdm, "person",
                                   person)

  observation_period <- dplyr::tibble(
    person_id = c(1L,1L,2L,3L),
    observation_period_start_date = as.Date(c("2010-01-01",
                                            "2019-01-01",
                                            "2019-01-01",
                                            "2019-01-01")),
    observation_period_end_date = as.Date(c("2016-01-01",
                                          "2021-01-01",
                                          "2022-01-01",
                                          "2019-01-01")),
    observation_period_id = c(1L,2L,3L, 4L),
    period_type_concept_id = 0L
  )
  cdm <- omopgenerics::insertTable(cdm, "observation_period",
                                   observation_period)
  my_cohort <- dplyr::tibble(
    cohort_definition_id = c(1L,2L,1L,1L),
    subject_id =c(1L,1L,2L,3L),
    cohort_start_date = as.Date(c("2020-01-01",
                                  "2015-05-12",
                                  "2020-01-01",
                                  "2020-01-01")),
    cohort_end_date = as.Date(c("2020-01-01",
                                "2015-05-12",
                                "2020-01-01",
                                "2020-01-01"))
  )
  cdm <- omopgenerics::insertTable(cdm, "my_cohort",
                                   my_cohort)
  # note we have a cohort entry outside of observation to test expected NA
  cdm$my_cohort <- omopgenerics::newCohortTable(cdm$my_cohort,
                                                .softValidation = TRUE)

  cdm$my_cohort_obs <- cdm$my_cohort |>
    addObservationPeriodId()
  expect_true("observation_period_id" %in%
                colnames(cdm$my_cohort_obs))

  expect_true(cdm$my_cohort_obs |>
                dplyr::filter(subject_id == 1) |>
                dplyr::filter(cohort_definition_id == 1L) |>
                dplyr::pull("observation_period_id") == 2L )
  expect_true(cdm$my_cohort_obs |>
                dplyr::filter(subject_id == 1) |>
                dplyr::filter(cohort_definition_id == 2L) |>
                dplyr::pull("observation_period_id") == 1L )
  expect_true(cdm$my_cohort_obs |>
                      dplyr::filter(subject_id == 2) |>
                      dplyr::pull("observation_period_id") == 3L )
  expect_true(is.na(cdm$my_cohort_obs |>
    dplyr::filter(subject_id == 3) |>
    dplyr::pull("observation_period_id")))

  # overwrite variable if it already exists
 expect_warning(cdm$my_cohort_obs <- cdm$my_cohort_obs |>
    addObservationPeriodId())

  # cannot add to the observation period table
expect_error(cdm$observation_period |>
               addObservationPeriodId(indexDate = "observation_period_end_date"))

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
 cdm$drug_exposure |>
   dplyr::filter(person_id == 0) |>
   addObservationPeriodId(indexDate = "drug_exposure_start_date")

 mockDisconnect(cdm = cdm)

})
