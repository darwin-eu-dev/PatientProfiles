test_that("test user define table", {
  test_table1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3),
    subject_id = c(1, 1, 1, 1),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  test_table2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3),
    subject_id = 1,
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  cdm1 <- mockPatientProfiles(connectionDetails, test_table1 = test_table1)
  x <- cdm1$test_table1 %>% dplyr::collect() |> dplyr::as_tibble()
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  expect_true(all.equal(x, test_table1))

  cdm2 <- mockPatientProfiles(connectionDetails, test_table1 = test_table1, test_table2 = test_table2)
  x <- cdm2$test_table1 %>% dplyr::collect() |> dplyr::as_tibble()
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  expect_true(all.equal(x, test_table1))
  x <- cdm2$test_table2 %>% dplyr::collect() |> dplyr::as_tibble()
  attr(x, "cohort_set") <- NULL
  attr(x, "cohort_attrition") <- NULL
  expect_true(all.equal(x, test_table2))
})

test_that("check working example with defaults", {
  cdm <- mockPatientProfiles(connectionDetails)

  expect_true(nrow(cdm$drug_exposure %>% dplyr::collect()) == 10)
  expect_true(nrow(cdm$person %>% dplyr::collect()) == 1)
})

test_that("check dug exposure and patient table size", {
  cdm <- mockPatientProfiles(connectionDetails, drug_exposure_size = 200, patient_size = 200)

  expect_true(nrow(cdm$drug_exposure %>% dplyr::collect()) == 200)
  expect_true(nrow(cdm$person %>% dplyr::collect()) == 200)
})

test_that("add cdm with person, cohort1 and observation_period", {
  expect_no_error(cdm <- mockPatientProfiles(connectionDetails,
    person = dplyr::tibble(
      person_id = c(1, 2, 3, 4),
      gender_concept_id = c(8507, 8532, 8532, 8507),
      year_of_birth = c(1993, 1991, 1995, 1998),
      month_of_birth = c(4, 7, 8, 10),
      day_of_birth = c(19, 5, 10, 22),
      race_concept_id = 0,
      ethnicity_concept_id = 0
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2, 2),
      subject_id = c(1, 2, 3, 3, 4, 4, 1),
      cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2018-01-01", "2020-01-01")),
      cohort_end_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2019-01-01", "2022-01-01")),
    ),
    observation_period = dplyr::tibble(
      observation_period_id = c(1, 2, 3, 4),
      person_id = c(1, 2, 3, 4),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2005-12-15", "1998-12-31", "2012-08-19"
      )),
      observation_period_end_date = as.Date(c(
        "2028-01-01", "2025-12-15", "2033-12-31", "2022-08-19"
      )),
      period_type_concept_id = 0
    )
  ))
})

test_that("attributes for cohort table", {
  test_table1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 3),
    subject_id = 1,
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  test_table1 <- addCohortCountAttr(test_table1)

  cdm <- mockPatientProfiles(connectionDetails, test_table1 = test_table1)

  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cdm$cohort1))
  ))

  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cdm$cohort2))
  ))

  expect_true(all(
    c("cohort_set", "cohort_attrition") %in% names(attributes(cdm$test_table1))
  ))
})
