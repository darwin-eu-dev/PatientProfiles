# must be changed once PriorHistory is updated in CohortProfiles online

test_that("addDemographics, input length and type", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

  expect_error(addDemographics(2,cdm))
  expect_error(addDemographics(cdm$cohort1, cdm$concept_ancestor))
  expect_error(addDemographics(cdm$cohort1,cdm,demographicsAt = "condition_start_date"))
  expect_error(addDemographics(cdm$cohort1,cdm,demographicsAt = c("cohort_start_date","cohort_end_date")))
  expect_error(addDemographics(cdm$cohort1,cdm,ageGroup = 10))
  expect_error(addDemographics(cdm$cohort1,cdm,compute = c(FALSE,TRUE)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, cohort and condition_occurrence", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addDemographics(cdm,demographicsAt = "condition_start_date")

  expect_true(all(c("age","sex","prior_history") %in% colnames(cdm$cohort1)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(age) %>% dplyr::pull() == c(60,53,58,53)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Female","Male","Female")))
  expect_true(all(cdm$cohort1 %>% dplyr::select(prior_history) %>% dplyr::pull() == c(234,1586,-492,1434)))
  expect_true(all(c("age","sex","prior_history") %in% colnames(cdm$condition_occurrence)))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(age) %>% dplyr::pull() == c(34,59,29,49,85,32,51,41,54,80)))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Female","Female","Female","Male","Female","Male","Male","Female","Male")))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(prior_history) %>% dplyr::pull() == c(-2238,70,-506,32,-1792,-5146,-2713,-1850,-4769,-844)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, parameters", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))
  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm,demographicsAt = "cohort_end_date",ageGroup = list(c(0,40),c(41,120)))

  expect_true(all(c("age","sex","prior_history","ageGroupNames") %in% colnames(cdm$cohort1)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(age) %>% dplyr::pull() == c(53,58,60,53)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Male","Female","Female")))
  expect_true(all(cdm$cohort1 %>% dplyr::select(prior_history) %>% dplyr::pull() == c(1525,-432,265,1647)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(ageGroupNames) %>% dplyr::pull() == c("41;120","41;120","41;120","41;120")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

