test_that("addInObservtaion, Inf windows, completeInterval T", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_no_error(
    cdm$cohort1 %>%
      PatientProfiles::addInObservation(
        window = c(0, Inf),
        completeInterval = T
      )
  )

  expect_no_error(
    cdm$cohort1 %>%
      PatientProfiles::addInObservation(
        window = c(-Inf, 0),
        completeInterval = T
      )
  )

  mockDisconnect(cdm = cdm)
})

test_that("addDemographics, input length, type", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 10
  )

  expect_error(addDemographics(2))
  expect_error(addDemographics(cdm$cohort1, indexDate = "condition_start_date"))
  expect_error(addDemographics(cdm$cohort1, indexDate = c("cohort_start_date", "cohort_end_date")))
  expect_error(addDemographics(cdm$cohort1, ageGroup = 10))
  expect_error(addDemographics(cdm$cohort1, age = FALSE, sex = FALSE, priorObservation = FALSE, futureObservation = FALSE))
})

test_that("addDemographics, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 10
  )

  oldcohort <- cdm$cohort1
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(ageImposeMonth = TRUE, ageImposeDay = TRUE)
  cdm$condition_occurrence <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageImposeMonth = TRUE,
      ageImposeDay = TRUE
    )

  expect_true(length(attributes(cdm$cohort1)) == length(attributes(oldcohort)))
  for (i in names(attributes(cdm$cohort1))) {
    if (i != "names" && i != "tbl_name" && i != "cdm_reference") {
      x <- attr(cdm$cohort1, i)
      y <- attr(oldcohort, i)
      if (i == "class") {
        x <- x[x != "GeneratedCohortSet"]
        y <- y[y != "GeneratedCohortSet"]
      }
      expect_true(identical(x, y))
    }
  }

  expect_true(all(c("age", "sex", "prior_observation") %in% colnames(cdm$cohort1)))
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 6 & .data$cohort_start_date == as.Date("2073-01-03")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 79L)
  expect_true(s$sex == "Female")
  expect_true(s$prior_observation == 28857L)

  expect_true(all(
    c("age", "sex", "prior_observation") %in% colnames(cdm$condition_occurrence)
  ))
  x <- cdm$condition_occurrence |>
    dplyr::collect() |>
    dplyr::arrange(.data$person_id, .data$condition_start_date)
  expect_identical(
    x$age,
    as.integer(c(
      65, 114, 19, 109, 19, 31, 55, 106, 54, 58, 2, 53, 101, 10, 69, 84
    ))
  )
  expect_identical(
    x$sex,
    c("Male", "Male", "Female", "Female", "Female", "Female", "Female",
      "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male")
  )
  expect_identical(
    x$prior_observation,
    as.integer(c(
      23818, 41917, 7236, 40009, 7168, 11616, 20311, 38951, 19822, 21484, 784,
      19480, 37156, 3921, 25377, 30726
    ))
  )

  mockDisconnect(cdm = cdm)
})

test_that("addDemographics, parameters", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = dplyr::tibble(
      person_id = c(1, 3),
      year_of_birth = c(1998, 1998),
      month_of_birth = 4L,
      day_of_birth = 1L,
      gender_concept_id = 8532L,
      race_concept_id = 0L,
      ethnicity_concept_id = 0L
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = c(1L, 1L, 3L),
      cohort_start_date = as.Date(c("2020-01-01", "2020-06-01", "2050-01-01")),
      cohort_end_date = cohort_start_date
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1:2,
      person_id = c(1, 3),
      observation_period_start_date = as.Date(c("2006-05-09", "2010-01-01")),
      observation_period_end_date = as.Date(c("2028-05-09", "2055-01-01")),
      period_type_concept_id = 0L
    )
  )
  cdm$cohort1 <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      ageGroup = list("age_group" = list(c(0, 40), c(41, Inf))),
      ageImposeMonth = TRUE,
      ageImposeDay = TRUE
    )

  expect_true(all(
    c("age", "sex", "prior_observation", "age_group") %in% colnames(cdm$cohort1)
  ))
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-01-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 22)
  expect_true(s$sex == "Female")
  expect_true(s$prior_observation == 4985)
  expect_true(s$age_group == "0 to 40")
  s <- cdm$cohort1 %>%
    dplyr::filter(
      .data$subject_id == 1 & .data$cohort_start_date == as.Date("2020-06-01")
    ) %>%
    dplyr::collect()
  expect_true(s$age == 22)
  expect_true(s$sex == "Female")
  expect_true(s$prior_observation == 5137)
  expect_true(s$age_group == "0 to 40")
  s <- cdm$cohort1 %>%
    dplyr::filter(.data$subject_id == 3) %>%
    dplyr::collect()
  expect_true(s$age == 52)
  expect_true(s$sex == "Female")
  expect_true(s$prior_observation == 14610)
  expect_true(s$age_group == "41 or above")
})

test_that("partial demographics - cohorts", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 10
  )

  # only age
  cdm$cohort1a <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = TRUE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE
    )
  # age and age group
  expect_equal(
    colnames(cdm$cohort1a),
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "age")
  )

  # only sex
  cdm$cohort1b <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = FALSE,
      ageGroup = NULL,
      sex = TRUE,
      priorObservation = FALSE,
      futureObservation = FALSE
    )
  expect_equal(
    colnames(cdm$cohort1b),
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "sex")
  )

  # only prior history
  cdm$cohort1c <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = FALSE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = FALSE
    )
  expect_equal(
    colnames(cdm$cohort1c),
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "prior_observation")
  )

  # only future observation
  cdm$cohort1d <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = FALSE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = TRUE
    )
  expect_equal(
    colnames(cdm$cohort1d),
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "future_observation")
  )


  # all
  cdm$cohort1e <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = TRUE,
      ageGroup = list(c(0, 100)),
      sex = TRUE,
      priorObservation = TRUE,
      futureObservation = TRUE
    )
  # age and age group
  expect_equal(
    colnames(cdm$cohort1e),
    c("cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date", "age", "sex", "prior_observation",
      "future_observation", "age_group")
  )
})

test_that("partial demographics - omop tables", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 10
  )

  # only age
  cdm$condition_occurrence1a <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      age = TRUE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = FALSE
    )
  # age and age group
  expect_true(c("age") %in% colnames(cdm$condition_occurrence1a))
  expect_true(all(!c("sex", "age_group", "prior_observation") %in%
    colnames(cdm$condition_occurrence1a)))

  # only sex
  cdm$cohort1b <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = FALSE,
      ageGroup = NULL,
      sex = TRUE,
      priorObservation = FALSE
    )
  expect_true(c("sex") %in% colnames(cdm$cohort1b))
  expect_true(all(!c("age", "age_group", "prior_observation") %in%
    colnames(cdm$cohort1b)))

  # only prior history
  cdm$cohort1c <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_end_date",
      age = FALSE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = TRUE
    )
  expect_true(c("prior_observation") %in% colnames(cdm$cohort1c))
  expect_true(all(!c("age", "age_group", "sex") %in%
    colnames(cdm$cohort1c)))

  # all
  cdm$condition_occurrence1d <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      age = TRUE,
      ageGroup = list(c(0, 100)),
      sex = TRUE,
      priorObservation = TRUE
    )
  # age and age group
  expect_true(all(c("age", "sex", "prior_observation")
  %in% colnames(cdm$condition_occurrence1d)))
})

test_that("priorObservation and future_observation - outside of observation period", {
  # priorObservation should be NA if index date is outside of an observation period
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:2,
    person_id = 1:2,
    condition_concept_id = 0,
    condition_start_date = as.Date(c("2000-02-01")),
    condition_end_date = as.Date(c("2001-02-01")),
    condition_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    condition_occurrence = condition_occurrence,
    observation_period = dplyr::tibble(
      observation_period_id = 1:2,
      person_id = 1:2,
      observation_period_start_date = as.Date("2005-01-01"),
      observation_period_end_date = as.Date("2025-01-01"),
      period_type_concept_id = 0L
    )
  )

  cdm$condition_occurrence <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      age = FALSE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = TRUE
    )
  # both should be missing
  expect_true(all(is.na(cdm$condition_occurrence %>% dplyr::pull(prior_observation))))
  expect_true(all(is.na(cdm$condition_occurrence %>% dplyr::pull(future_observation))))
})

test_that("priorObservation - multiple observation periods", {
  # with multiple observation periods,
  # prior history should relate to the most recent observation start date

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = 1980,
    month_of_birth = 01,
    day_of_birth = 01,
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
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
    ),
    period_type_concept_id = 0
  )
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    observation_period = observation_period,
    cohort1 = cohort1,
    cohort2 = cohort1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_start_date",
      age = FALSE,
      ageGroup = NULL,
      sex = FALSE,
      priorObservation = TRUE,
      futureObservation = TRUE
    )
  expect_true(nrow(cdm$cohort1a %>% dplyr::collect()) == 2)
  expect_true(all(cdm$cohort1a %>% dplyr::pull(prior_observation) ==
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
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-01-01")),
    cohort_end_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-01-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    observation_period_start_date = as.Date(c("2015-06-30", "2019-06-30", "2021-06-30")),
    observation_period_end_date = as.Date(c("2018-06-30", "2020-06-30", "2022-06-30")),
    period_type_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    observation_period = observation_period,
    cohort2 = cohort1
  )
  # using temp
  cdm$cohort1_new <- cdm$cohort1 %>%
    addDemographics(
      indexDate = "cohort_start_date",
      age = TRUE,
      ageGroup = list(c(10, 100)),
      sex = FALSE,
      priorObservation = FALSE,
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
    addDemographics(
      indexDate = "cohort_start_date",
      age = FALSE,
      sex = TRUE,
      priorObservation = FALSE,
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
    addDemographics(
      indexDate = "cohort_start_date",
      age = FALSE,
      sex = FALSE,
      priorObservation = TRUE,
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
    addDemographics(
      indexDate = "cohort_start_date",
      age = FALSE,
      sex = FALSE,
      priorObservation = FALSE,
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

test_that("age at cohort end, no missing, check age computation", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:2,
    cohort_start_date = as.Date(c("2002-11-30", "2002-12-02")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:2,
    person_id = 1:2,
    observation_period_start_date = as.Date("2001-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )

  person <- dplyr::tibble(
    person_id = 1:2,
    gender_concept_id = 8507L,
    year_of_birth = 2001L,
    month_of_birth = 12L,
    day_of_birth = 1L,
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    observation_period = observation_period,
    cohort2 = cohort1
  )

  # check if exact age is computed, ie, dob 2000-01-01, target date 2000-12-01  --> age 0
  # dob 2000-01-01, target date 2001-01-02  --> age 1
  result <- cdm[["cohort1"]] |>
    addAge(ageImposeMonth = FALSE, ageImposeDay = FALSE) %>%
    dplyr::collect()

  expect_true(result %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::pull("age") == 0)
  expect_true(result %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::pull("age") == 1)

  result <- addDemographics(
    x = cdm[["cohort1"]],
    ageImposeMonth = FALSE,
    ageImposeDay = FALSE
  ) %>%
    dplyr::collect()
  expect_true(result %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::pull("age") == 0)
  expect_true(result %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::pull("age") == 1)
})

test_that("age at cohort entry, missing year/month/day of birth", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2010-03-03", "2010-03-01", "2010-02-01")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01", "2013-01-01"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )

  person <- dplyr::tibble(
    person_id = 1:3,
    gender_concept_id = 8507,
    year_of_birth = c(2000, NA, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    cohort2 = cohort1,
    observation_period = observation_period
  )

  result <- addAge(
    x = cdm$cohort1, ageImposeMonth = FALSE, ageImposeDay = FALSE,
    ageMissingMonth = 4, ageMissingDay = 4
  ) %>% dplyr::collect()

  expect_true(all(c(colnames(cohort1), "age") %in% colnames(result)))
  expect_equal(nrow(result), 3)
  expect_true(all(c(9, NA) %in% result$age))

  resultB <- addDemographics(
    x = cdm$cohort1, ageImposeMonth = FALSE, ageImposeDay = FALSE,
    ageMissingMonth = 4, ageMissingDay = 4,
    sex = FALSE,
    priorObservation = FALSE, futureObservation = FALSE,
  ) %>% dplyr::collect()

  expect_equal(result, resultB)
})

test_that("multiple cohortIds, check age at cohort end", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1:3,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2009-12-01", "2010-01-01", "2010-01-01")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01", "2018-01-01"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )

  person <- dplyr::tibble(
    person_id = 1:3,
    gender_concept_id = c(8507, 8532, 8507),
    year_of_birth = c(2000, 2000, NA),
    month_of_birth = c(NA, 01, 01),
    day_of_birth = c(01, 01, 01),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    cohort2 = cohort1,
    observation_period = observation_period
  )

  result <- cdm[["cohort1"]] |>
    addAge(indexDate = "cohort_end_date") |>
    dplyr::collect()


  expect_true(all(c("1", "2", "3") %in% result$subject_id))
  expect_true(all(c(15, 13, NA) %in% result$age))

  resultB <- cdm$cohort1 |>
    addDemographics(
      indexDate = "cohort_end_date",
      sex = FALSE,
      priorObservation = FALSE,
      futureObservation = FALSE,
    ) %>%
    dplyr::collect()

  expect_equal(result, resultB)
})

test_that("age group checks", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1:3,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2009-12-01", "2010-01-01", "2010-01-01")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01", "2018-01-01"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )

  person <- dplyr::tibble(
    person_id = 1:3,
    gender_concept_id = c(8507, 8532, 8507),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(3, 7, NA),
    day_of_birth = c(NA, 02, 01),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    cohort2 = cohort1,
    observation_period = observation_period
  )

  x <- cdm$cohort1 %>%
    addAge()

  result1a <- x %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result1b <- addDemographics(
    cdm$cohort1,
    ageGroup = list("age_group" = list(c(1, 20), c(21, 30), c(31, 40))),
    sex = FALSE,
    priorObservation = FALSE, futureObservation = FALSE
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(all(result1a$age_group == c("1 to 20", "21 to 30", "31 to 40")))
  expect_equal(result1a, result1b)

  # change the order of ageGroup provided, result should be the same
  result2a <- x %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(21, 30), c(1, 20), c(31, 40)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result3a <- cdm$cohort1 %>%
    addAge(ageGroup = list(c(1, 20), c(21, 30), c(31, 40))) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$age)
  expect_true(identical(result1a, result2a))
  expect_true(identical(result1a, result3a))

  result2b <- cdm$cohort1 %>%
    addDemographics(
      ageGroup = list("age_group" = list(c(21, 30), c(1, 20), c(31, 40))),
      sex = FALSE,
      priorObservation = FALSE, futureObservation = FALSE
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  result3b <- addDemographics(
    cdm$cohort1,
    ageGroup = list("age_group" = list(c(1, 20), c(21, 30), c(31, 40))),
    sex = FALSE,
    priorObservation = FALSE, futureObservation = FALSE
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(identical(result1b, result2b))
  expect_true(identical(result1b, result3b))

  # if age has missing values
  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1:3,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2009-12-01", "2010-01-01", "2010-01-01")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01", "2018-01-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )
  person <- dplyr::tibble(
    person_id = 1:3,
    gender_concept_id = c(8507, 8532, 8507),
    year_of_birth = c(NA, 1970, 2000),
    month_of_birth = c(3, 7, NA),
    day_of_birth = c(NA, 02, 01),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    cohort2 = cohort1,
    observation_period = observation_period
  )
  result1 <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      "age", list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(
    result1 %>%
      dplyr::filter(is.na(age)) %>%
      dplyr::pull("age_group") %>%
      is.na()
  )

  # not all ages in age group
  result2 <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      "age", list("age_group" = list(c(35, 45)))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)
  expect_true(result2 %>%
    dplyr::filter(age == 10) %>%
    dplyr::pull("age_group") == "None")
})

test_that("age variable names", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  result <- addAge(
    x = cdm[["cohort1"]],
    ageName = "current_age",
    indexDate = "cohort_end_date"
  ) %>%
    addDemographics(
      ageName = "working_age",
      sex = FALSE,
      priorObservation = FALSE, futureObservation = FALSE
    ) %>%
    dplyr::collect()
  expect_true(all(c("current_age", "working_age") %in% colnames(result)))
})

test_that("expected errors", {
  # check input length and type for each of the arguments
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 1,
    numberIndividuals = 5
  )

  expect_error(addAge("cdm$cohort1"))
  expect_error(addAge(cdm$cohort1, indexDate = "subject_id"))
  expect_error(expect_error(addAge(cdm$cohort1,
    indexDate = "cohort_start_date",
    ageMissingMonth = "1"
  )))
  expect_error(expect_error(addAge(cdm$cohort1,
    indexDate = "cohort_start_date",
    ageMissingDay = "1"
  )))
  expect_error(addAge(cdm$cohort1,
    indexDate = "cohort_start_date",
    ageImposeMonth = "TRUE"
  ))
  expect_error(addAge(cdm$cohort1,
    indexDate = "cohort_start_date",
    ageImposeDay = "TRUE"
  ))

  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  expect_error(result <- addAge())
  expect_error(result <- addAge(
    x = cdm[["cohort1"]],
    ageImposeDay = 1
  ))
  expect_error(result <- addAge(
    x = cdm[["cohort1"]],
    ageImposeMonth = 1
  ))
  expect_error(result <- addAge(
    x = cdm[["cohort1"]],
    indexDate = "date"
  ))
  expect_error(result <- addAge(
    x = cdm[["cohort1"]],
    ageMissingMonth = 1.1
  ))
  expect_error(result <- addAge(
    x = cdm[["cohort1"]],
    ageMissingDay = 1.1
  ))


  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:3,
    cohort_start_date = as.Date(c("2010-03-03", "2010-03-01", "2010-02-01")),
    cohort_end_date = as.Date(c("2015-01-01", "2013-01-01", "2013-01-01"))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2020-01-01"),
    period_type_concept_id = 0
  )

  person <- dplyr::tibble(
    person_id = 1:3,
    gender_concept_id = c(8507, 8507, 8507),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = person,
    cohort1 = cohort1,
    cohort2 = cohort1,
    observation_period = observation_period
  )

  cdm$cohort1 <- cdm$cohort1 %>% addAge()

  # error if overlapping ageGroups
  expect_error(addCategories(
    cdm$cohort1,
    "age",
    list("age_group" = list(c(1, 22), c(19, 30), c(31, 40)))
  ))

  # throw error if length of vector in agegroup is not 2
  expect_error(addCategories(
    cdm$cohort1,
    "age",
    list("age_group" = list(c(1, 2, 3)))
  ))

  # if x does not have "age" column, it has to be in cdm
  expect_error(addCategories(
    cdm$cohort2,
    "age",
    list("age_group" = list(c(1, 2)))
  ))
})

test_that("addCategories input", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 1,
    numberIndividuals = 5
  )

  # overwrite when categories named same as variable, throw warning
  expect_error(
    cdm$cohort1 %>%
      addAge() %>%
      addCategories(
        variable = "age",
        categories = list("age" = list(c(1, 30), c(31, 99)))
      )
  )

  expect_error(
    cdm$cohort1 %>%
      addDemographics(
        sex = FALSE,
        priorObservation = FALSE,
        futureObservation = FALSE,
        ageGroup = list("age" = list(c(1, 30), c(31, 40)))
      )
  )

  # default group name when no input
  expect_true("category_1" %in% colnames(cdm$cohort1 %>% addAge() %>%
    addCategories(
      variable = "age",
      categories = list(list(c(1, 30), c(31, 40)))
    )))
  # Error when x is not a tibble
  expect_error(c(1, 2, 3, 4) %>% addCategories(
    variable = "age",
    categories = list(list(c(1, 30), c(31, 40)))
  ))

  result <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      variable = "age",
      categories = list(
        list(c(1, 30), c(31, 40)),
        list(c(0, 50), c(51, 100))
      )
    ) %>%
    dplyr::collect()
  expect_true(all(c("category_1", "category_2") %in% colnames(result)))

  # ERROR when repeat group name
  expect_error(cdm$cohort1 %>% addAge() %>%
    addCategories(
      variable = "age",
      categories = list(
        "age_A" = list(c(0, 30), c(31, 120)),
        "age_A" = list(c(1, 18), c(19, 40))
      )
    ))

  expect_error(
    cdm$cohort1 %>%
      addDemographics(
        sex = FALSE,
        priorObservation = FALSE,
        futureObservation = FALSE,
        ageGroup = list(
          "age_A" = list(c(0, 30), c(31, 120)),
          "age_A" = list(c(1, 18), c(19, 40))
        )
      )
  )

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
    "age" = c(1, 1, 1, 1, 1),
    sex = c(1, 1, 1, 1, 1),
    prior_observation = c(1, 1, 1, 1, 1),
    future_observation = c(1, 1, 1, 1, 1)
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

  observation_period <- dplyr::tibble(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2025-01-01"),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    observation_period = observation_period
  )

  expect_warning(
    result <- cdm$cohort1 %>%
      addDemographics() %>%
      dplyr::collect()
  )

  expect_true(sum(colnames(result) == "age") == 1)
  expect_true(sum(colnames(result) == "sex") == 1)
  expect_true(sum(colnames(result) == "prior_observation") == 1)
  expect_true(sum(colnames(result) == "future_observation") == 1)

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(age) !=
    cohort1 %>%
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(age), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(sex) !=
    cohort1 %>%
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(sex), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(prior_observation) !=
    cohort1 %>%
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(prior_observation), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(future_observation) !=
    cohort1 %>%
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(future_observation), na.rm = TRUE))
})

test_that("date of birth", {
  person <- dplyr::tibble(
    person_id = 1:2,
    gender_concept_id = 8507,
    year_of_birth = c(2001, 2005),
    month_of_birth = c(12, 06),
    day_of_birth = c(01, 15),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), person = person
  )

  personDOB <- cdm$person %>%
    addDateOfBirth() %>%
    dplyr::collect()
  expect_true(personDOB %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-12-01")
  expect_true(personDOB %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-06-15")

  drug_exposure_dob <- cdm$drug_exposure %>%
    addDateOfBirth() %>%
    dplyr::collect()
  expect_true(all(drug_exposure_dob %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-12-01"))
  expect_true(all(drug_exposure_dob %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-06-15"))

  cohort_dob <- cdm$cohort1 %>%
    addDateOfBirth() %>%
    dplyr::collect()
  expect_true(cohort_dob %>% dplyr::filter(subject_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-12-01")
  expect_true(cohort_dob %>% dplyr::filter(subject_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-06-15")


  personDOB2 <- cdm$person %>%
    addDateOfBirth(imposeDay = TRUE, imposeMonth = TRUE) %>%
    dplyr::collect()
  expect_true(personDOB2 %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-01-01")
  expect_true(personDOB2 %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-01-01")

  drug_exposure_dob2 <- cdm$drug_exposure %>%
    addDateOfBirth(imposeDay = TRUE, imposeMonth = TRUE) %>%
    dplyr::collect()
  expect_true(all(drug_exposure_dob2 %>% dplyr::filter(person_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-01-01"))
  expect_true(all(drug_exposure_dob2 %>% dplyr::filter(person_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-01-01"))

  cohortDOB2 <- cdm$cohort1 %>%
    addDateOfBirth(imposeDay = TRUE, imposeMonth = TRUE) %>%
    dplyr::collect()
  expect_true(cohortDOB2 %>% dplyr::filter(subject_id == 1) %>% dplyr::pull(date_of_birth) ==
    "2001-01-01")
  expect_true(cohortDOB2 %>% dplyr::filter(subject_id == 2) %>% dplyr::pull(date_of_birth) ==
    "2005-01-01")
})

test_that("missing levels", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  result <- cdm[["cohort1"]] %>%
    addDemographics(
      ageGroup = list(c(0, 25)),
      sex = FALSE,
      priorObservation = FALSE, futureObservation = FALSE
    ) %>%
    dplyr::collect()
  expect_true("None" %in% result$age_group)
  expect_true(all(is.na(result$age_group[is.na(result$age)])))

  result <- cdm$cohort1 %>%
    addSex() %>%
    dplyr::collect()
  expect_true(all(!is.na(result$sex)))

  result <- cdm$person %>%
    dplyr::mutate(gender_concept_id = "111") %>%
    addSex() %>%
    dplyr::collect()
  expect_true(all(!is.na(result$sex)))
})

test_that("overwriting obs period variables", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  cdm$cohort1 <- cdm$cohort1 %>%
    addDateOfBirth() |>
    addDemographics()
  expect_true(all(c(
    "date_of_birth", "age", "sex", "prior_observation", "future_observation"
  ) %in% colnames(cdm$cohort1)))

  cdm$cohort2 <- cdm$cohort2 %>%
    dplyr::mutate(observation_period_start_date = "a") |>
    addPriorObservation() |>
    addFutureObservation() |>
    addInObservation()

  expect_true(all(c(
    "observation_period_start_date", "prior_observation", "future_observation",
    "in_observation"
  ) %in% colnames(cdm$cohort2)))
  expect_identical(
    cdm$cohort2 |> dplyr::pull("observation_period_start_date") |> unique(),
    "a"
  )

})

test_that("addDemographics, date of birth option", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_no_error(x <- cdm$cohort1 |> addDemographics(dateOfBirth = T))
  expect_true("date_of_birth" %in% colnames(x))
  expect_no_error(
    x <- cdm$cohort1 |>
      addDemographics(dateOfBirth = T, dateOfBirthName = "abc")
  )
  expect_true("abc" %in% colnames(x))
  expect_false("date_of_birth" %in% colnames(x))
  expect_no_error(x <- cdm$cohort1 |> addDemographics(dateOfBirth = F))
  expect_false("date_of_birth" %in% colnames(x))
  mockDisconnect(cdm = cdm)
})

test_that("allow NA as age_group", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_no_error(
    cdm$cohort1 <- cdm$cohort1 |>
      addAge(ageGroup = list(c(0,0)), missingAgeGroupValue = NA_character_)
  )
  expect_true(all(is.na(cdm$cohort1 |> dplyr::pull("age_group"))))
  mockDisconnect(cdm = cdm)
})

test_that("allow age_group only", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_no_error(
    cdm$cohort1 <- cdm$cohort1 |>
      addDemographics(
        age = FALSE,
        ageGroup = list(c(0, 39), c(40, Inf)),
        sex = FALSE,
        priorObservation = FALSE,
        futureObservation = FALSE
      )
  )
  expect_true("age_group" %in% colnames(cdm$cohort1))
  expect_false("age" %in% colnames(cdm$cohort1))
  mockDisconnect(cdm = cdm)
})
