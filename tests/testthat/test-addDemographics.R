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
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-01-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5295)
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-06-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5447)
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 2) %>%
    dplyr::collect()
  expect_true(s$age == 60)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5221)
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 3) %>%
    dplyr::collect()
  expect_true(s$age == 58)
  expect_true(s$sex == "Male")
  expect_true(s$prior_history == 4562)

  expect_true(all(c("age","sex","prior_history") %in% colnames(cdm$condition_occurrence)))
  expected_age <- c(43, 58, 54, 39, 53, 39, 31, 97, 40, 78)
  expected_sex <- c(
    "Female", "Female", "Male", "Female", "Female", "Female", "Female", "Male",
    "Male", "Male"
  )
  expected_prior_history <- c(
    1711, 4714, 3160, 2304, -390, 3553, 2763, 5145, 697, 3209
  )
  for (k in 1:length(expected_age)) {
    expect_true(
      cdm$condition_occurrence %>%
        dplyr::filter(.data$subject_id == k) %>%
        dplyr::pull("age") == expected_age[k]
    )
    expect_true(
      cdm$condition_occurrence %>%
        dplyr::filter(.data$subject_id == k) %>%
        dplyr::pull("sex") == expected_sex[k]
    )
    expect_true(
      cdm$condition_occurrence %>%
        dplyr::filter(.data$subject_id == k) %>%
        dplyr::pull("prior_history") == expected_prior_history[k]
    )
  }

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, parameters", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10)
  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm,demographicsAt = "cohort_end_date",ageGroup = list(c(0,40),c(41,120)))

  expect_true(all(c("age","sex","prior_history","ageGroupNames") %in% colnames(cdm$cohort1)))
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-01-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5386)
  expect_true(s$ageGroupNames == "41;120")
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-06-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5508)
  expect_true(s$ageGroupNames == "41;120")
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 2) %>%
    dplyr::collect()
  expect_true(s$age == 60)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5252)
  expect_true(s$ageGroupNames == "41;120")
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 3) %>%
    dplyr::collect()
  expect_true(s$age == 58)
  expect_true(s$sex == "Male")
  expect_true(s$prior_history == 4622)
  expect_true(s$ageGroupNames == "41;120")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

