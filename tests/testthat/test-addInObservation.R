test_that("addInObservation, different settings", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

  result1 <- addInObservation(cdm$cohort1,cdm)
  result2 <- addInObservation(cdm$cohort2,cdm)
  result3 <- addInObservation(cdm$cohort1 %>% dplyr::rename(person_id = subject_id),cdm)

  expect_true("in_observation" %in% colnames(result1))
  expect_true(all(result1 %>% dplyr::select(in_observation) %>% dplyr::pull() == c(TRUE,TRUE,FALSE,TRUE)))
  expect_true("in_observation" %in% colnames(result1))
  expect_true(all(result2 %>% dplyr::select(in_observation) %>% dplyr::pull() == c(TRUE,FALSE,TRUE,TRUE,TRUE)))
  expect_true(all(result1 %>% dplyr::select(in_observation) %>% dplyr::pull() == result3 %>% dplyr::select(in_observation) %>% dplyr::pull()))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

  expect_error(addInObservation(2,cdm))
  expect_error(addInObservation(cdm$cohort2,"cdm"))
  expect_error(addInObservation(cdm$person,cdm))
  expect_error(addInObservation(cdm$cohort1,cdm,observationAt = 3))
  expect_error(addInObservation(cdm$cohort1,cdm,observationAt = "2002-01-02"))
  expect_error(addInObservation(cdm$cohort1,cdm,observationAt = c("cohort","cohort_end")))
  expect_error(addInObservation(cdm$cohort2,cdm,name = 3))
  expect_error(addInObservation(cdm$cohort2,cdm,name = c("name1","name2")))
  expect_error(addInObservation(cdm$cohort2,cdm,compute = 2))
  expect_error(addInObservation(cdm$cohort2,cdm,compute = c(TRUE,FALSE)))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

