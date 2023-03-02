# must be changed once PriorHistory is updated in CohortProfiles online

test_that("addDemographics, input length and type", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10)

  expect_error(addDemographics(2,cdm))
  expect_error(addDemographics(cdm$cohort1, cdm$concept_ancestor))
  expect_error(addDemographics(cdm$cohort1,cdm,demographicsAt = "condition_start_date"))
  expect_error(addDemographics(cdm$cohort1,cdm,demographicsAt = c("cohort_start_date","cohort_end_date")))
  expect_error(addDemographics(cdm$cohort1,cdm,ageGroup = 10))
  expect_error(addDemographics(cdm$cohort1,cdm,compute = c(FALSE,TRUE)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, cohort and condition_occurrence", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10)

  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addDemographics(cdm,demographicsAt = "condition_start_date")

  expect_true(all(c("age","sex","prior_history") %in% colnames(cdm$cohort1)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(age) %>% dplyr::pull() == c(60,53,58,53)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Female","Male","Female")))
  expect_true(all(cdm$cohort1 %>% dplyr::select(prior_history) %>% dplyr::pull() == c(5221,5447,4562,5295)))
  expect_true(all(c("age","sex","prior_history") %in% colnames(cdm$condition_occurrence)))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(age) %>% dplyr::pull() == c(39,58,31,43,97,39,54,40,53,78)))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Female","Female","Female","Male","Female","Male","Male","Female","Male")))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(prior_history) %>% dplyr::pull() == c(3553, 4714, 2763, 1711, 5145, 2304, 3160,  697, -390, 3209)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, parameters", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10)
  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm,demographicsAt = "cohort_end_date",ageGroup = list(c(0,40),c(41,120)))

  expect_true(all(c("age","sex","prior_history","ageGroupNames") %in% colnames(cdm$cohort1)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(age) %>% dplyr::pull() == c(53,58,60,53)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(sex) %>% dplyr::pull() == c("Female","Male","Female","Female")))
  expect_true(all(cdm$cohort1 %>% dplyr::select(prior_history) %>% dplyr::pull() == c(5386,4622,5252,5508)))
  expect_true(all(cdm$cohort1 %>% dplyr::select(ageGroupNames) %>% dplyr::pull() == c("41;120","41;120","41;120","41;120")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

