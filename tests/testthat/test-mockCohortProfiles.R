test_that("test user define table", {

  test_table_1 <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  test_table_2 <- tibble::tibble(
    cohort_definition_id = c(2, 2, 2, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cdm_1 <- mockCohortProfiles(user_table_name = "test_table_1",test_table_1 = test_table_1)
  cdm_2 <- mockCohortProfiles(user_table_name = c("test_table_1","test_table_2"),test_table_1 = test_table_1,test_table_2 = test_table_2)

expect_true(dplyr::all_equal(cdm_1$test_table_1 %>% dplyr::collect(),test_table_1))
expect_true(length(cdm_1)==10)
expect_error(mockCohortProfiles(user_table_name = c("test_table_1","test_table_2"),test_table_1 = test_table_1))
expect_true(length(cdm_2)==11)
expect_true(dplyr::all_equal(cdm_2$test_table_1 %>% dplyr::collect(),test_table_1))
expect_true(dplyr::all_equal(cdm_2$test_table_2 %>% dplyr::collect(),test_table_2))

DBI::dbDisconnect(attr(cdm_1, "dbcon"), shutdown = TRUE)
DBI::dbDisconnect(attr(cdm_2, "dbcon"), shutdown = TRUE)
})

test_that("check working example with cohort table", {

  test_table_1 <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cdm_1 <- mockCohortProfiles(cohort1 = test_table_1)
  cdm_2 <- mockCohortProfiles(cohort2 = test_table_1)

  expect_true(dplyr::all_equal(cdm_1$cohort1 %>% dplyr::collect(),test_table_1))
  expect_error(expect_true(dplyr::all_equal(cdm_1$cohort2 %>% dplyr::collect(),test_table_1)))
  expect_true(dplyr::all_equal(cdm_2$cohort2 %>% dplyr::collect(),test_table_1))
  expect_error(expect_true(dplyr::all_equal(cdm_2$cohort1 %>% dplyr::collect(),test_table_1)))

  DBI::dbDisconnect(attr(cdm_1, "dbcon"), shutdown = TRUE)
  DBI::dbDisconnect(attr(cdm_2, "dbcon"), shutdown = TRUE)

})


test_that("check working example with defaults", {
  cdm <- mockCohortProfiles()

  expect_true(length(cdm)==9)
  expect_true(nrow(cdm$drug_exposure %>% dplyr::collect())==10)
  expect_true(nrow(cdm$person %>% dplyr::collect())==1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("check dug exposure and patient table size", {
  cdm <- mockCohortProfiles(drug_exposure_size = 200, patient_size = 200)

  expect_true(length(cdm)==9)
  expect_true(nrow(cdm$drug_exposure %>% dplyr::collect())==200)
  expect_true(nrow(cdm$person %>% dplyr::collect())==200)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
