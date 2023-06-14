test_that("test user define table", {
  testTable1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  testTable2 <- dplyr::tibble(
    cohort_definition_id = c(2, 2, 2, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  cdm1 <- mockPatientProfiles(connectionDetails, testTable1 = testTable1)
  expect_true(all.equal(cdm1$testTable1 %>% dplyr::collect(), testTable1))

  cdm2 <- mockPatientProfiles(connectionDetails, testTable1 = testTable1, testTable2 = testTable2)
  expect_true(all.equal(cdm2$testTable1 %>% dplyr::collect(), testTable1))
  expect_true(all.equal(cdm2$testTable2 %>% dplyr::collect(), testTable2))
})

test_that("check working example with cohort table", {
  testTable1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  cdm1 <- mockPatientProfiles(connectionDetails, cohort1 = testTable1)
  expect_true(all.equal(cdm1$cohort1 %>% dplyr::collect(), testTable1))
  expect_error(expect_true(all.equal(cdm1$cohort2 %>% dplyr::collect(), testTable1)))

  cdm2 <- mockPatientProfiles(connectionDetails, cohort2 = testTable1)
  expect_true(all.equal(cdm2$cohort2 %>% dplyr::collect(), testTable1))
  expect_error(expect_true(all.equal(cdm2$cohort1 %>% dplyr::collect(), testTable1)))
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
  cdm <- mockPatientProfiles(connectionDetails,
    person = dplyr::tibble(
      person_id = c(1, 2, 3, 4),
      gender_concept_id = c(8507, 8532, 8532, 8507),
      year_of_birth = c(1993, 1991, 1995, 1998),
      month_of_birth = c(4, 7, 8, 10),
      day_of_birth = c(19, 5, 10, 22)
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2, 2),
      subject_id = c(1, 2, 3, 3, 4, 4, 5),
      cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
      cohort_end_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01")),
    ),
    observation_period = dplyr::tibble(
      observation_period_id = c(1, 2, 3, 4),
      person_id = c(1, 2, 3, 4),
      observation_period_start_date = as.Date(c(
        "2000-01-01", "2005-12-15", "1998-12-31", "2012-08-19"
      )),
      observation_period_end_date = as.Date(c(
        "2028-01-01", "2025-12-15", "2033-12-31", "2022-08-19"
      ))
    )
  )
})

test_that("attributes for cohort table", {
  testTable1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01")
    )
  )

  testTable1 <- addCohortCountAttr(testTable1)


  cdm <- mockPatientProfiles(connectionDetails, testTable1 = testTable1)

  expect_true(all(names(attributes(cdm$cohort1)) %in%
    c("names", "class", "cdm_reference",
      "cohort_set", "cohort_attrition", "cohort_count")))

  expect_true(all(names(attributes(cdm$cohort2)) %in%
    c("names", "class", "cdm_reference",
      "cohort_set", "cohort_attrition", "cohort_count")))

  expect_true(all(names(attributes(cdm$testTable1)) %in%
    c("names", "class", "cdm_reference",
      "cohort_set", "cohort_attrition", "cohort_count")))
})
