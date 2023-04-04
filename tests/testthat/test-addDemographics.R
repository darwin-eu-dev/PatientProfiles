test_that("addDemographics, input length and type", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)

  expect_error(addDemographics(2, cdm))
  expect_error(addDemographics(cdm$cohort1, cdm$concept_ancestor))
  expect_error(addDemographics(cdm$cohort1, cdm, indexDate = "condition_start_date"))
  expect_error(addDemographics(cdm$cohort1, cdm, indexDate = c("cohort_start_date", "cohort_end_date")))
  expect_error(addDemographics(cdm$cohort1, cdm, ageGroup = 10))
  expect_error(addDemographics(cdm$cohort1, cdm, tablePrefix = 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)

  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addDemographics(cdm, indexDate = "condition_start_date")

  expect_true(all(c("age", "sex", "prior_history") %in% colnames(cdm$cohort1)))
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

  expect_true(all(c("age", "sex", "prior_history") %in% colnames(cdm$condition_occurrence)))
  expected_age <- c(43, 58, 54, 39, 53, 39, 31, 97, 40, 78)
  expected_sex <- c(
    "Female", "Female", "Male", "Female", "Female", "Female", "Female", "Male",
    "Male", "Male"
  )
  expected_prior_history <- c(
    1711, 4714, 3160, 2304, NA, 3553, 2763, 5145, 697, 3209
  )
  for (k in 1:length(expected_age)) {
    expect_true(
      cdm$condition_occurrence %>%
        dplyr::filter(.data$person_id == k) %>%
        dplyr::pull("age") == expected_age[k]
    )
    expect_true(
      cdm$condition_occurrence %>%
        dplyr::filter(.data$person_id == k) %>%
        dplyr::pull("sex") == expected_sex[k]
    )
    expect_true(
      if(!is.na(expected_prior_history[k])){
        cdm$condition_occurrence %>%
          dplyr::filter(.data$person_id == k) %>%
          dplyr::pull("prior_history") == expected_prior_history[k]
      } else {
        is.na(cdm$condition_occurrence %>%
          dplyr::filter(.data$person_id == k) %>%
          dplyr::pull("prior_history"))
      }

    )
  }

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addDemographics, parameters", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(cdm, indexDate = "cohort_end_date",
                    ageGroup = list("age_group" = list(c(0, 40), c(41, 120))))

  expect_true(all(c("age", "sex", "prior_history", "age_group") %in% colnames(cdm$cohort1)))
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-01-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5386)
  expect_true(s$age_group == "41 to 120")
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-06-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 53)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5508)
  expect_true(s$age_group == "41 to 120")
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 2) %>%
    dplyr::collect()
  expect_true(s$age == 60)
  expect_true(s$sex == "Female")
  expect_true(s$prior_history == 5252)
  expect_true(s$age_group == "41 to 120")
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 3) %>%
    dplyr::collect()
  expect_true(s$age == 58)
  expect_true(s$sex == "Male")
  expect_true(s$prior_history == 4622)
  expect_true(s$age_group == "41 to 120")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("partial demographics - cohorts", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)

  # only age
  cdm$cohort1a <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = TRUE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = FALSE,
                    futureObservation = FALSE
    )
  # age and age group
  expect_equal(
    names(cdm$cohort1a),
    c("cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "age")
  )

  # only sex
  cdm$cohort1b <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = TRUE,
                    priorHistory = FALSE,
                    futureObservation = FALSE
    )
  expect_equal(
    names(cdm$cohort1b),
    c("cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "sex")
  )

  # only prior history
  cdm$cohort1c <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = TRUE,
                    futureObservation = FALSE
    )
  expect_equal(
    names(cdm$cohort1c),
    c("cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "prior_history")
  )

  # only future observation
  cdm$cohort1d <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = FALSE,
                    futureObservation = TRUE
    )
  expect_equal(
    names(cdm$cohort1d),
    c("cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "future_observation")
  )


  # all
  cdm$cohort1e <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = TRUE,
                    ageGroup = list(c(0, 100)),
                    sex = TRUE,
                    priorHistory = TRUE,
                    futureObservation = TRUE
    )
  # age and age group
  expect_equal(
    names(cdm$cohort1e),
    c("cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
       "age", "sex", "prior_history",
       "future_observation", "age_group")
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("partial demographics - omop tables", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)

  # only age
  cdm$condition_occurrence1a <- cdm$condition_occurrence %>%
    addDemographics(cdm,
                    indexDate = "condition_start_date",
                    age = TRUE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = FALSE
    )
  # age and age group
  expect_true(c("age") %in% names(cdm$condition_occurrence1a))
  expect_true(all(!c("sex", "age_group", "prior_history") %in%
                    names(cdm$condition_occurrence1a)))

  # only sex
  cdm$cohort1b <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = TRUE,
                    priorHistory = FALSE
    )
  expect_true(c("sex") %in% names(cdm$cohort1b))
  expect_true(all(!c("age", "age_group", "prior_history") %in%
                    names(cdm$cohort1b)))

  # only prior history
  cdm$cohort1c <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = TRUE
    )
  expect_true(c("prior_history") %in% names(cdm$cohort1c))
  expect_true(all(!c("age", "age_group", "sex") %in%
                    names(cdm$cohort1c)))

  # all
  cdm$condition_occurrence1d <- cdm$condition_occurrence %>%
    addDemographics(cdm,
                    indexDate = "condition_start_date",
                    age = TRUE,
                    ageGroup = list(c(0, 100)),
                    sex = TRUE,
                    priorHistory = TRUE
    )
  # age and age group
  expect_true(all(c("age", "sex", "prior_history")
                  %in% names(cdm$condition_occurrence1d)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("priorHistory and future_observation - outside of observation period", {

  # priorHistory should be NA if index date is outside of an observation period

  person <- tibble::tibble(
    person_id = c(1,2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c(1,2),
    person_id = c(1, 2),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2014-01-01")),
    observation_period_end_date = c(as.Date("2001-01-01"),
                                    as.Date("2015-01-01"))
  )
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1,2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(person = person,
                             observation_period = observation_period,
                             cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = TRUE,
                    futureObservation = TRUE
    )
  # both should be NA
  expect_true(all(is.na(cdm$cohort1a %>% dplyr::pull(prior_history))))
  expect_true(all(is.na(cdm$cohort1a %>% dplyr::pull(future_observation))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})

test_that("priorHistory - multiple observation periods", {

  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- tibble::tibble(
    person_id = c(1,2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c(1,2, 3),
    person_id = c(1,1, 2),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2010-01-01"),
                                      as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2005-01-01"),
                                    as.Date("2015-01-01"),
                                    as.Date("2015-01-01"))
  )
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1,2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(person = person,
                             observation_period = observation_period,
                             cohort1 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = FALSE,
                    ageGroup = NULL,
                    sex = FALSE,
                    priorHistory = TRUE,
                    futureObservation = TRUE
    )
  expect_true(nrow(cdm$cohort1a %>% dplyr::collect()) == 2)
  expect_true(all(cdm$cohort1a %>% dplyr::pull(prior_history) ==
                    as.numeric(difftime(as.Date("2012-02-01"),
                                        as.Date("2010-01-01"),
                                        units = "days"))))
  expect_true(all(cdm$cohort1a %>% dplyr::pull(future_observation) ==
                    as.numeric(difftime(as.Date("2015-01-01"),
                                        as.Date("2012-02-01"),
                                        units = "days"))))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})

test_that("check that no extra rows are added", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 1, 2, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01", "2022-01-01")),
    cohort_end_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01", "2022-01-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    observation_period_start_date = as.Date(c("2015-06-30", "2019-06-30", "2021-06-30")),
    observation_period_end_date = as.Date(c("2018-06-30", "2020-06-30", "2022-06-30"))
  )
  cdm <- mockPatientProfiles(
    cohort1 = cohort1, observation_period = observation_period
  )
  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = TRUE,
                    ageGroup = list(c(10,100)),
                    sex = FALSE,
                    priorHistory = FALSE,
                    futureObservation = FALSE
    )

  # temp tables created by dbplyr
  expect_true(
    cdm$cohort1_new %>% dplyr::tally() %>% dplyr::pull() ==
      cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull()
  )

  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = FALSE,
                    sex = TRUE,
                    priorHistory = FALSE,
                    futureObservation = FALSE
    )

  # temp tables created by dbplyr
  expect_true(
    cdm$cohort1_new %>% dplyr::tally() %>% dplyr::pull() ==
      cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull()
  )

  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = FALSE,
                    sex = FALSE,
                    priorHistory = TRUE,
                    futureObservation = FALSE
    )

  # temp tables created by dbplyr
  expect_true(
    cdm$cohort1_new %>% dplyr::tally() %>% dplyr::pull() ==
      cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull()
  )

  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_start_date",
                    age = FALSE,
                    sex = FALSE,
                    priorHistory = FALSE,
                    futureObservation = TRUE
    )

  # temp tables created by dbplyr
  expect_true(
    cdm$cohort1_new %>% dplyr::tally() %>% dplyr::pull() ==
      cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull()
  )


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("temp and permanent tables", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)
  # using temp
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = TRUE,
                    ageGroup = list(c(0, 100)),
                    sex = TRUE,
                    priorHistory = TRUE
    )
  # temp tables created by dbplyr
  expect_true(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                                      "dbplyr_")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)
  # using permanent
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(cdm,
                    indexDate = "cohort_end_date",
                    age = TRUE,
                    ageGroup = list(c(0, 100)),
                    sex = TRUE,
                    priorHistory = TRUE,
                    tablePrefix = "my_perm"
    )
  # permanent table
  expect_true(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                                      "my_perm_")))
  # no temp tables created by dbplyr
  expect_true(!any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                                       "dbplyr_")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
