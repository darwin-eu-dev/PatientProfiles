test_that("addFollowUp, input length and type", {
  expect_error(addFollowUp(2))
  expect_error(addFollowUp(cohort_table, start = c("cohort_start_date","cohort_end_date")))
  expect_error(addFollowUp(cohort_table, start = "name"))
  expect_error(addFollowUp(cohort_table, end = c("cohort_start_date","cohort_end_date")))
  expect_error(addFollowUp(cohort_table, end = FALSE))
  expect_error(addFollowUp(cohort_table, name = c("follow","up")))
  expect_error(addFollowUp(cohort_table, name = 20))
  expect_error(addFollowUp(cohort_table, name = "subject_id"))

})

test_that("addFollowUp, cohort and condition_occurrence", {
  cdm <- mockCohortProfiles(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

  cdm$cohort1 <- cdm$cohort1 %>% addFollowUp()
  expect_true("follow_up" %in% colnames(cdm$cohort1))
  expect_true(all(cdm$cohort1 %>% dplyr::select(follow_up) %>% dplyr::pull() == c(92,62,32,61)))

  cdm$condition_occurrence <- cdm$condition_occurrence %>% addFollowUp(start = "condition_start_date", end = "condition_end_date")

  expect_true("follow_up" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence %>% dplyr::select(follow_up) %>% dplyr::pull() == c(274,357,947,453,443,780,668,938,248,934)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addFollowUp, parameters", {
  cohort_table <- tibble::tibble(
    subject_id = c(1:5),
    cohort_definition_id = c(1),
    cohort_start_date = c(as.Date("2016-11-12"),as.Date("2017-10-10"),as.Date("2020-01-01"),as.Date("1987-05-23"),as.Date("1998-06-02")),
    cohort_end_date = c(as.Date("2018-11-12"),as.Date("2017-10-18"),as.Date("2022-02-11"),as.Date("1996-04-21"),as.Date("1998-06-02"))
  )

  cdm <- mockCohortProfiles(patient_size = 5, cohort1 = cohort_table)
  test1 <- cdm$cohort1 %>% addFollowUp(start = "cohort_end_date")

  expect_true("follow_up" %in% colnames(test1))
  expect_true(all(test1 %>% dplyr::select(follow_up) %>% dplyr::pull() == c(1,1,1,1,1)))

  test2 <- cdm$cohort1 %>% addFollowUp(name = "followup", start = "cohort_end_date")

  expect_true("followup" %in% colnames(test2))
  expect_true(all(c(1,1,1,1,1) == test2 %>% dplyr::select(followup) %>% dplyr::pull()))

  test3 <- cdm$cohort1 %>% addFollowUp(name = "follow_up2")
  expect_true(all(test3 %>% dplyr::select(follow_up2) %>% dplyr::pull() == c(731,9,773,3257,1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

