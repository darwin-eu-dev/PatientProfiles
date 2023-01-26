test_that("sex, no missing",{
  cdm <- mockCohortProfiles(seed = 27, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort1 <- addSex(cdm$cohort1,cdm)
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true(all(cdm$cohort1$sex == c("Male","Male","Male","Male")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("gender, missing",{
  cdm <- mockCohortProfiles(seed = 27, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$person <- cdm$person %>% dplyr::mutate(gender_concept_id = ifelse(gender_concept_id == "8507", "99",gender_concept_id))
  cdm$cohort2 <- addSex(cdm$cohort2,cdm,name="gender")
  expect_true("gender" %in% colnames(cdm$cohort2))
  expect_true(all(cdm$cohort2$gender == c("NA","NA","NA","NA")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("sex, person_id",{
  cdm <- mockCohortProfiles(seed = 8, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort1 <- addSex(cdm$cohort1 %>% dplyr::mutate(person_id = subject_id) %>% dplyr::select(-c(subject_id)),cdm)
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true("person_id" %in% colnames(cdm$cohort1))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors",{
  cdm <- mockCohortProfiles(seed = 100, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  expect_error(addSex("cdm$cohort1",cdm))
  expect_error(addSex(cdm$cohort1,"cdm"))
  expect_error(addSex(cdm$drug_strength,cdm))
  expect_error(addSex(cdm$cohort1,cdm,name = 2))
  expect_error(addSex(cdm$cohort1,cdm, name = c("name1","name2")))
  expect_error(addSex(cdm$cohort1,cdm, compute = "compute"))
  expect_error(addSex(cdm,cdm))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
