test_that("check input length and type for each of the arguments", {
  cdm <-
    mockCohortProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2019-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )

  expect_error(addPriorHistory("cdm$cohort1", cdm))

  expect_error(addPriorHistory(cdm$cohort1, "cdm"))

  expect_error(addPriorHistory(cdm$cohort1, cdm, priorHistoryAt = "end_date"))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})

test_that("check condition_occurrence and cohort1 work", {

#mock data
  cdm <-
    mockCohortProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2005-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )
#check it works with cohort1 table in mockdb
  expect_true(typeof(cdm$cohort1 %>% addPriorHistory(cdm) %>% dplyr::collect()) == "list")
  expect_true("prior_history" %in% colnames(cdm$cohort1 %>% addPriorHistory(cdm)))
#check it works with condition_occurrence table in mockdb
  expect_true(typeof(cdm$condition_occurrence %>% addPriorHistory(cdm,priorHistoryAt = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("prior_history" %in% colnames(cdm$condition_occurrence %>% addPriorHistory(cdm,priorHistoryAt = "condition_start_date")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("check working example with cohort1",{
#create mock tables for testing
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  obs_1 <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-02-03"),
      as.Date("2010-02-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2014-01-01"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )

  cdm <-
    mockCohortProfiles(
      seed = 1,
      cohort1 = cohort1,
      observation_period = obs_1

    )

  result <- cdm$cohort1 %>% addPriorHistory(cdm) %>% dplyr::collect()

  expect_true(all(colnames(cohort1) %in% colnames(result)))

  expect_true(all(result %>% dplyr::select("prior_history") == tibble::tibble(prior_history = c(28,28,31))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check working example with condition_occurrence", {
  #create mock tables for testing
  condition_occurrence <- tibble::tibble(
    condition_occurrence_id = c("1", "1", "1"),
    person_id = c("1", "2", "3"),
    condition_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    condition_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  obs_1 <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-02-03"),
      as.Date("2010-02-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2014-01-01"),
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )

  cdm <-
    mockCohortProfiles(
      seed = 1,
      condition_occurrence = condition_occurrence,
      observation_period = obs_1

    )

  result <-
    cdm$condition_occurrence %>% addPriorHistory(cdm, priorHistoryAt = "condition_start_date") %>% dplyr::collect()

  expect_true(all(
    colnames(
      condition_occurrence %>% dplyr::rename(subject_id = person_id)
    ) %in% colnames(result)
  ))

  expect_true(all(
    result %>% dplyr::select("prior_history") == tibble::tibble(prior_history = c(28, 28, 31))
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
