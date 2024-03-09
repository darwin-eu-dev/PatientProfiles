test_that("summariseCohortTiming", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=2000),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=30),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5),
                   sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5)),
    cohort_start_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                  rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                  rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5))),
    cohort_end_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5)))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:20,
    person_id = 1:20,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date =  as.Date("2025-01-01"),
    period_type_concept_id = NA
  )

  cdm <- mockPatientProfiles(person = person, observation_period = obs, table = table)

  timing1 <- summariseCohortTiming(cdm$table,
                                    restrictToFirstEntry = TRUE)

  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing1))

  expect_true(all(timing1$estimate_name |> unique() %in%
                    c("min", "q25","median","q75","max","count", "mean", "sd")))
  expect_equal(timing1$estimate_value[1], timing1$estimate_value[2])

  timing2 <- summariseCohortTiming(cdm$table,
                                    restrictToFirstEntry = FALSE,
                                    timing = c("min",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing2))
  expect_false(timing2$estimate_value[5] == timing2$estimate_value[6])
  expect_true(all(timing2$estimate_name |> unique() %in%
                    c("min","max","count")))

  CDMConnector::cdm_disconnect(cdm)

})
test_that("tableCohortTiming", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=2000),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=30),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5),
                   sample(1:20, 5), sample(1:20, 5), sample(1:20, 5), sample(1:20, 5)),
    cohort_start_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                  rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                  rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5))),
    cohort_end_date = as.Date(c(rep("2000-01-01",5), rep("2010-09-05",5), rep("2006-05-01",5),
                                rep("2003-03-31",5), rep("2008-07-02",5), rep("2000-01-01",5),
                                rep("2012-09-05",5), rep("1996-05-01",5), rep("1989-03-31",5)))
  )

  obs <- dplyr::tibble(
    observation_period_id = 1:20,
    person_id = 1:20,
    observation_period_start_date = as.Date("1930-01-01"),
    observation_period_end_date =  as.Date("2025-01-01"),
    period_type_concept_id = NA
  )

  cdm <- mockPatientProfiles(person = person, observation_period = obs, table = table)

  timing1 <- summariseCohortTiming(cdm$table,
                                   restrictToFirstEntry = TRUE)
  tibble1 <- tableCohortTiming(timing1, type = "tibble")
  expect_true(all(c("Cdm name", "Cohort name reference", "Cohort name comparator", "Variable name", "Estimate name", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(all(unique(tibble1$`Cohort name comparator`) %in%
                    unique(tibble1$`Cohort name reference`)))


  tibble2 <- tableCohortTiming(timing1, type = "tibble", cohortNameReference = "cohort_1")
  expect_true("cohort_1" == unique(tibble2$`Cohort name reference`))

  gt1 <- tableCohortTiming(timing1, type = "gt", cohortNameReference = "cohort_1")
  expect_true("gt_tbl" %in% class(gt1))

  CDMConnector::cdm_disconnect(cdm)

})
