test_that("addDemographics, input length, type, tableprefix", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

  expect_error(addDemographics(2, cdm))
  expect_error(addDemographics(cdm$cohort1, cdm$concept_ancestor))
  expect_error(addDemographics(cdm$cohort1, cdm, indexDate = "condition_start_date"))
  expect_error(addDemographics(cdm$cohort1, cdm, indexDate = c("cohort_start_date", "cohort_end_date")))
  expect_error(addDemographics(cdm$cohort1, cdm, ageGroup = 10))
  expect_error(addDemographics(cdm$cohort1, cdm, tablePrefix = 1))
  expect_error(addDemographics(cdm$cohort1, cdm, age = FALSE, sex = FALSE, priorHistory = FALSE, futureObservation = FALSE))
})

test_that("addDemographics, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

  oldcohort <- cdm$cohort1
  cdm$cohort1 <- cdm$cohort1 %>% addDemographics(cdm, ageImposeMonth = TRUE, ageImposeDay = TRUE)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addDemographics(cdm, indexDate = "condition_start_date", ageImposeMonth = TRUE, ageImposeDay = TRUE)

  expect_true(length(attributes(cdm$cohort1)) == length(attributes(oldcohort)))
  for(i in names(attributes(cdm$cohort1))) {
    if(i != "names" && i != "class") {
      expect_true(identical(attr(cdm$cohort1, i), attr(oldcohort, i)))
    }
  }

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
      if (!is.na(expected_prior_history[k])) {
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


})

test_that("addDemographics, parameters", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(cdm,
      indexDate = "cohort_end_date",
      ageGroup = list("age_group" = list(c(0, 40), c(41, 120))),
      ageImposeMonth = TRUE,
      ageImposeDay = TRUE
    )

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


})

test_that("partial demographics - cohorts", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

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
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "age"
    )
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
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "sex"
    )
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
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "prior_history"
    )
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
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "future_observation"
    )
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
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "age", "sex", "prior_history",
      "future_observation", "age_group"
    )
  )


})

test_that("partial demographics - omop tables", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

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


})

test_that("priorHistory and future_observation - outside of observation period", {

  # priorHistory should be NA if index date is outside of an observation period

  person <- tibble::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2014-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2001-01-01"),
      as.Date("2015-01-01")
    )
  )
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    connectionDetails,
    person = person,
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


})

test_that("priorHistory - multiple observation periods", {

  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- tibble::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 2),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-01-01"),
      as.Date("2015-01-01"),
      as.Date("2015-01-01")
    )
  )
  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    connectionDetails,
    person = person,
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
      units = "days"
    ))))
  expect_true(all(cdm$cohort1a %>% dplyr::pull(future_observation) ==
    as.numeric(difftime(as.Date("2015-01-01"),
      as.Date("2012-02-01"),
      units = "days"
    ))))



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
    connectionDetails, cohort1 = cohort1, observation_period = observation_period
  )
  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(cdm,
      indexDate = "cohort_start_date",
      age = TRUE,
      ageGroup = list(c(10, 100)),
      sex = FALSE,
      priorHistory = FALSE,
      futureObservation = FALSE
    )

  # temp tables created by dbplyr
  expect_true(
    cdm$cohort1_new %>% dplyr::tally() %>% dplyr::pull() ==
      cdm$cohort1 %>%
        dplyr::tally() %>%
        dplyr::pull()
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
      cdm$cohort1 %>%
        dplyr::tally() %>%
        dplyr::pull()
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
      cdm$cohort1 %>%
        dplyr::tally() %>%
        dplyr::pull()
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
      cdm$cohort1 %>%
        dplyr::tally() %>%
        dplyr::pull()
  )



})

test_that("temp and permanent tables", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)
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
  expect_true(any(stringr::str_starts(
    CDMConnector::listTables(attr(cdm, "dbcon")),
    "dbplyr_"
  )))


  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)
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
  expect_true(any(stringr::str_starts(
    CDMConnector::listTables(attr(cdm, "dbcon")),
    "my_perm_"
  )))
  # no temp tables created by dbplyr
  expect_true(!any(stringr::str_starts(
    CDMConnector::listTables(attr(cdm, "dbcon")),
    "dbplyr_"
  )))

})

test_that("age at cohort end, no missing, check age computation", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2002-11-30"), as.Date("2002-12-02")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8507"),
    year_of_birth = c(2001, 2001),
    month_of_birth = c(12, 12),
    day_of_birth = c(01, 01)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

  # check if exact age is computed, ie, dob 2000-01-01, target date 2000-12-01  --> age 0
  # dob 2000-01-01, target date 2001-01-02  --> age 1
  result <- addAge(
    x = cdm[["cohort1"]], cdm = cdm,
    ageImposeMonth = FALSE,
    ageImposeDay = FALSE
  ) %>%
    dplyr::collect()
  expect_true(identical(result$age, c(0, 1)))

  result <- addDemographics(
    x = cdm[["cohort1"]], cdm = cdm,
    ageImposeMonth = FALSE,
    ageImposeDay = FALSE
  ) %>%
    dplyr::collect()
  expect_true(identical(result$age, c(0, 1)))


})

test_that("age at cohort entry, missing year/month/day of birth", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(2000, NA, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

  result <- addAge(
    x = cdm$cohort1, cdm = cdm, ageImposeMonth = FALSE, ageImposeDay = FALSE,
    ageDefaultMonth = 4, ageDefaultDay = 4
  ) %>% dplyr::collect()

  expect_true(all(c(colnames(cohort1), "age") %in% colnames(result)))
  expect_equal(nrow(result), 3)
  expect_true(identical(result$age, c(9, 9, NA)))

  result_b <- addDemographics(
    x = cdm$cohort1, cdm = cdm, ageImposeMonth = FALSE, ageImposeDay = FALSE,
    ageDefaultMonth = 4, ageDefaultDay = 4,
    sex = FALSE,
    priorHistory = FALSE, futureObservation = FALSE,
  ) %>% dplyr::collect()

  expect_equivalent(result, result_b)


})

test_that("multiple cohortIds, check age at cohort end", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "2", "3"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2009-12-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8532", "8507"),
    year_of_birth = c(2000, 2000, NA),
    month_of_birth = c(NA, 01, 01),
    day_of_birth = c(01, 01, 01)
  )
  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

  result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                   indexDate = "cohort_end_date") %>%
    dplyr::collect()

  expect_true(identical(result$subject_id, c("1", "2", "3")))
  expect_true(identical(result$age, c(15, 13, NA)))

  result_b <- addDemographics(
    x = cdm$cohort1, cdm = cdm, indexDate = "cohort_end_date",
    sex = FALSE,
    priorHistory = FALSE, futureObservation = FALSE,
  ) %>% dplyr::collect()

  expect_equivalent(result, result_b)


})

test_that("age group checks", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

  x <- cdm$cohort1 %>%
    addAge(cdm)

  result1a <- addCategories(
    x, cdm, "age", list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result1b <- addDemographics(
    x, cdm,
    ageGroup = list("age_group" = list(c(1, 20), c(21, 30), c(31, 40))),
    sex = FALSE,
    priorHistory = FALSE, futureObservation = FALSE
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(all(result1a$age_group == c("1 to 20", "21 to 30", "31 to 40")))
  expect_equivalent(result1a, result1b)

  # change the order of ageGroup provided, result should be the same
  result2a <- addCategories(
    x, cdm, "age", list("age_group" = list(c(21, 30), c(1, 20), c(31, 40)))
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result3a <- cdm$cohort1 %>%
    addAge(cdm, ageGroup = list(c(1, 20), c(21, 30), c(31, 40))) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$age)
  expect_true(identical(result1a, result2a))
  expect_true(identical(result1a, result3a))

  result2b <- addDemographics(
    x, cdm,
    ageGroup = list("age_group" = list(c(21, 30), c(1, 20), c(31, 40))),
    sex = FALSE,
    priorHistory = FALSE, futureObservation = FALSE
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result3b <- addDemographics(
    x, cdm,
    ageGroup = list("age_group" = list(c(1, 20), c(21, 30), c(31, 40))),
    sex = FALSE,
    priorHistory = FALSE, futureObservation = FALSE
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(identical(result1b, result2b))
  expect_true(identical(result1b, result3b))

  # if age has missing values
  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(NA, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)
  result1 <- cdm$cohort1 %>%
    addAge(cdm) %>%
    addCategories(
      cdm, "age", list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(is.na(result1 %>%
    dplyr::filter(is.na(age)) %>%
    dplyr::pull("age_group")))

  # not all ages in age group
  result2 <- cdm$cohort1 %>%
    addAge(cdm) %>%
    addCategories(
      cdm, "age", list("age_group" = list(c(35, 45)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(is.na(result2 %>%
    dplyr::filter(age == 10) %>%
    dplyr::pull("age_group")))


})

test_that("age variable names", {
  cdm <- mockPatientProfiles(connectionDetails)

  result <- addAge(
    x = cdm[["cohort1"]], cdm = cdm,
    ageName = "current_age",
    indexDate = "cohort_end_date"
  ) %>%
    addDemographics(cdm,
      ageName = "working_age",
      sex = FALSE,
      priorHistory = FALSE, futureObservation = FALSE
    ) %>%
    dplyr::collect()
  expect_true(all(c("current_age", "working_age") %in% colnames(result)))


})

test_that("expected errors", {
  # check input length and type for each of the arguments
  cdm <-
    mockPatientProfiles(
      connectionDetails,
      seed = 1,
      patient_size = 5
    )

  expect_error(addAge("cdm$cohort1", cdm))
  expect_error(addAge(cdm$cohort1, "cdm"))
  expect_error(addAge(cdm$cohort1, cdm, indexDate = "subject_id"))
  expect_error(expect_error(addAge(cdm$cohort1, cdm,
                                   indexDate = "cohort_start_date",
                                   ageDefaultMonth = "1")))
  expect_error(expect_error(addAge(cdm$cohort1, cdm,
                                   indexDate = "cohort_start_date",
                                   ageDefaultDay = "1")))
  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date",
                      ageImposeMonth = "TRUE"))
  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date",
                      ageImposeDay = "TRUE"))
  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date",
                      tablePrefix = 1))



  cdm <- mockPatientProfiles(connectionDetails)

  expect_error(result <- addAge(cdm = "a"))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                                ageImposeDay = 1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                                ageImposeMonth = 1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                                indexDate = "date"))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                                ageDefaultMonth = 1.1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm,
                                ageDefaultDay = 1.1))


  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

  cdm$cohort1 <- cdm$cohort1 %>% addAge(cdm)

  # error if overlapping ageGroups
  expect_error(addCategories(
    cdm$cohort1,
    cdm, "age",
    list("age_group" = list(c(1, 22), c(19, 30), c(31, 40)))
  ))

  # throw error if length of vector in agegroup is not 2
  expect_error(addCategories(
    cdm$cohort1,
    cdm, "age",
    list("age_group" = list(c(1, 2, 3)))
  ))

  # if x does not have "age" column, it has to be in cdm
  expect_error(addCategories(
    cdm$cohort2,
    cdm, "age",
    list("age_group" = list(c(1, 2)))
  ))


})

test_that("addCategories input",{

  cdm <-mockPatientProfiles(connectionDetails, seed = 1, patient_size = 5)

  # overwrite when categories named same as variable, throw warning
  expect_warning(cdm$cohort1 %>% addAge(cdm) %>%
                   addCategories(cdm, variable="age",
                                 categories= list("age" = list(c(1, 30), c(31, 99)))))

  expect_warning(cdm$cohort1 %>% addAge(cdm) %>%
                   addDemographics(cdm,
                                   sex = FALSE,
                                   priorHistory = FALSE,
                                   futureObservation = FALSE,
                                   ageGroup=list("age" = list(c(1, 30),  c(31, 40)))))

  # default group name when no input
  expect_true("category_1" %in% colnames(cdm$cohort1 %>% addAge(cdm) %>%
                                           addCategories(cdm, variable="age",
                                                         categories= list( list(c(1, 30),  c(31, 40))))))
  # Error when x is not a tibble
  expect_error(c(1,2,3,4) %>% addCategories(cdm, variable="age",
                                                          categories= list( list(c(1, 30),  c(31, 40)))))

  result <- cdm$cohort1 %>% addAge(cdm) %>%
    addCategories(cdm, variable="age",
                  categories=list( list(c(1, 30),  c(31, 40)),
                                   list(c(0,50),  c(51, 100)))) %>%
    dplyr::collect()
  expect_true(all(c("category_1", "category_2") %in% colnames(result)))

  # ERROR when repeat group name
  expect_error(cdm$cohort1 %>% addAge(cdm) %>%
                 addCategories(cdm ,
                               variable = "age",
                               categories = list("age_A" = list(c(0, 30), c(31, 120)),
                                                 "age_A" = list(c(1, 18), c(19, 40)))))

  expect_error(cdm$cohort1 %>% addAge(cdm) %>%
                 addDemographics(cdm,
                                 sex = FALSE,
                                 priorHistory = FALSE,
                                 futureObservation = FALSE,
                                 ageGroup = list("age_A" = list(c(0, 30), c(31, 120)),
                                                 "age_A" = list(c(1, 18),  c(19, 40)))))

  # Error when x is not a cdm object


})

test_that("test if column exist, overwrite", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    "age" = c(1,1,1,1,1),
    sex = c(1,1,1,1,1),
    prior_history = c(1,1,1,1,1),
    future_observation = c(1,1,1,1,1)
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>% addDemographics(cdm) %>% dplyr::collect()

  expect_true(sum(colnames(result) == "age") == 1)
  expect_true(sum(colnames(result) == "sex") == 1)
  expect_true(sum(colnames(result) == "prior_history") == 1)
  expect_true(sum(colnames(result) == "future_observation") == 1)

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(age) !=
                    cohort1 %>%
                    dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(age), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(sex) !=
                    cohort1 %>%
                    dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(sex), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(prior_history) !=
                    cohort1 %>%
                    dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(prior_history), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(future_observation) !=
                    cohort1 %>%
                    dplyr::arrange(cohort_start_date, subject_id) %>%
                    dplyr::select(future_observation), na.rm = TRUE))


})

test_that("date of birth", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2002-11-30"), as.Date("2002-12-02")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8507"),
    year_of_birth = c(2001, 2005),
    month_of_birth = c(12, 06),
    day_of_birth = c(01, 15)
  )

  cdm <- mockPatientProfiles(connectionDetails, person = person, cohort1 = cohort1)

 person_dob <- cdm$person %>%
    addDateOfBirth(cdm) %>%
    dplyr::collect()
 expect_true(person_dob %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
   "2001-12-01")
 expect_true(person_dob %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
               "2005-06-15")

 drug_exposure_dob <- cdm$drug_exposure %>%
   addDateOfBirth(cdm) %>%
   dplyr::collect()
 expect_true(all(drug_exposure_dob %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
               "2001-12-01"))
 expect_true(all(drug_exposure_dob %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
               "2005-06-15"))

 cohort_dob <- cdm$cohort1 %>%
   addDateOfBirth(cdm) %>%
   dplyr::collect()
 expect_true(cohort_dob %>% dplyr::filter(subject_id == 1) %>% dplyr::pull(date_of_birth) ==
               "2001-12-01")
 expect_true(cohort_dob %>% dplyr::filter(subject_id == 2) %>% dplyr::pull(date_of_birth) ==
               "2005-06-15")


 person_dob2 <- cdm$person %>%
   addDateOfBirth(cdm, imposeDay = TRUE, imposeMonth = TRUE) %>%
   dplyr::collect()
 expect_true(person_dob2 %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
               "2001-01-01")
 expect_true(person_dob2 %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
               "2005-01-01")

 drug_exposure_dob2 <- cdm$drug_exposure %>%
   addDateOfBirth(cdm, imposeDay = TRUE, imposeMonth = TRUE) %>%
   dplyr::collect()
 expect_true(all(drug_exposure_dob2 %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
                   "2001-01-01"))
 expect_true(all(drug_exposure_dob2 %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
                   "2005-01-01"))

 cohort_dob2 <- cdm$cohort1 %>%
   addDateOfBirth(cdm, imposeDay = TRUE, imposeMonth = TRUE) %>%
   dplyr::collect()
 expect_true(cohort_dob2 %>% dplyr::filter(subject_id == 1) %>% dplyr::pull(date_of_birth) ==
               "2001-01-01")
 expect_true(cohort_dob2 %>% dplyr::filter(subject_id == 2) %>% dplyr::pull(date_of_birth) ==
               "2005-01-01")


})
