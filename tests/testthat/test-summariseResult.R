test_that("test all functions", {
  x <- dplyr::tibble(
    s = c("g1", "g1", "g2", "g1 and g2", "g2", "g1 and g2"),
    v1 = c(1, 2, 3, 4, 6, 3),
    v2 = c("a", "b", "a and b", "b", "0", "0 and ab"),
    v3 = c(0, 1, 0, 1, 1, 0),
    v4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )
  s1 <- summariseResult(x)
  s2 <- summariseResult(x, strata = list("s"))
  s3 <- summariseResult(
    x,
    strata = list("s"), minCellCount = 1
  )
  s4 <- summariseResult(
    x,
    strata = list(c("s", "v2"), group2 = "s"),
    minCellCount = 1
  )

  x <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 1, 2),
    cohort_start_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    cohort_end_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    acetaminophen_m365_to_0 = c(1, 1, 0),
    ibuprophen_m365_to_0 = c(0, 0, 0),
    naloxone_m365_to_0 = c(0, 0, 0),
    headache_minf_to_0 = c(0, 1, 0),
    covid_minf_to_0 = c(1, 1, 0)
  )
  expect_no_error(summariseResult(
    x,
    strata = list(), minCellCount = 1
  ))

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(39, 40, 27, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(0, 1, 0, 0)
  )
  variables <- list(
    numeric = c(
      "age", "number_visits", "prior_history"
    ),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "percentage")
  )
  expect_no_error(
    result <- summariseResult(
      cohort,
      variables = variables, functions = functions
    )
  )
})

test_that("groups and strata", {
  cdm <- PatientProfiles::mockPatientProfiles(
    patient_size = 1000,
    drug_exposure_size = 1000
  )

  result <- cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(strata = list("sex"))

  expect_true(result %>%
    dplyr::filter(group_name == "Overall" &
      group_level == "Overall" &
      strata_name == "Overall" &
      strata_level == "Overall" &
      variable == "number subjects") %>%
    dplyr::pull("estimate") == "1000")


  result <- cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(strata = list(c("age_group", "sex")))

  expect_true(all(result %>%
    dplyr::select("strata_name") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c("Overall", "age_group and sex")))
  expect_true(all(result %>%
    dplyr::select("strata_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "Overall",
      "0 to 30 and Female",
      "0 to 30 and Male",
      "31 to 60 and Female",
      "31 to 60 and Male",
      "None and Female",
      "None and Male"
    )))

  result <- cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(group = list(c("age_group", "sex")))
  expect_true(all(result %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c("Overall", "age_group and sex")))
  expect_true(all(result %>%
    dplyr::select("group_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "Overall",
      "0 to 30 and Female",
      "0 to 30 and Male",
      "31 to 60 and Female",
      "31 to 60 and Male",
      "None and Female",
      "None and Male"
    )))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("table in db or local", {
  cdm <- PatientProfiles::mockPatientProfiles(
    patient_size = 1000,
    drug_exposure_size = 1000
  )

  # in db
  expect_no_error(cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    summariseResult(strata = list("sex")))

  # already collected
  expect_no_error(cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(strata = list("sex")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("with and with overall groups and strata", {
  cdm <- PatientProfiles::mockPatientProfiles(
    patient_size = 1000,
    drug_exposure_size = 1000
  )

  test_data <- cdm$condition_occurrence %>%
    addDemographics(cdm,
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect()

  expect_false(any(test_data %>%
    summariseResult(
      strata = list("sex"),
      includeOverallStrata = FALSE
    ) %>%
    dplyr::pull("strata_name") %in%
    c("Overall")))
  expect_true(any(test_data %>%
    summariseResult(
      strata = list("sex"),
      includeOverallStrata = TRUE
    ) %>%
    dplyr::pull("strata_name") %in%
    c("Overall")))

  expect_false(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = FALSE
    ) %>%
    dplyr::pull("group_name") %in%
    c("Overall")))
  expect_true(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = TRUE
    ) %>%
    dplyr::pull("group_name") %in%
    c("Overall")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("obscure", {
  x <- dplyr::tibble(
    s = c("g1", "g1", "g2", "g1&&g2", "g2", "g1&&g2"),
    v1 = c(1, 2, 3, 4, 6, 3),
    v2 = c("a", "b", "a&&b", "b", "0", "0&&ab"),
    v3 = c(0, 1, 0, 1, 1, 0),
    v4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )

  # minCellCount = 1
  s <- summariseResult(x, minCellCount = 1)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<1") == 0)
  expect_true(sum(is.na(s$estimate)) == 0)

  # minCellCount = 2
  s <- summariseResult(x, minCellCount = 2)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<2") == 4)
  expect_true(sum(is.na(s$estimate)) == 4)

  # minCellCount = 3
  s <- summariseResult(x, minCellCount = 3)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<3") == 8)
  expect_true(sum(is.na(s$estimate)) == 8)

  # minCellCount = 4
  s <- summariseResult(x, minCellCount = 4)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<4") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 5
  s <- summariseResult(x, minCellCount = 5)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<5") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 6
  s <- summariseResult(x, minCellCount = 6)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<6") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 7
  s <- summariseResult(x, minCellCount = 7)
  expect_true(nrow(s) == 25)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<7") == 1)
  expect_true(sum(is.na(s$estimate)) == 24)
})

test_that("test empty cohort", {
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )


  cdm <- mockPatientProfiles(
    connectionDetails,
    dus_cohort = dus_cohort
  )

  expect_no_error(
    cdm$dus_cohort %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = FALSE,
        includeOverallStrata = TRUE
      )
  )

  expect_no_error(
    cdm$dus_cohort %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = TRUE,
        includeOverallStrata = TRUE
      )
  )


  expect_no_error(
    cdm$dus_cohort %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = FALSE,
        includeOverallStrata = FALSE
      )
  )

  expect_no_error(
    cdm$dus_cohort %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = TRUE,
        includeOverallStrata = FALSE
      )
  )
})
