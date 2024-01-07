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
    numeric = c("median", "q25", "q75", "missing"),
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
    dplyr::filter(group_name == "overall" &
      group_level == "overall" &
      strata_name == "overall" &
      strata_level == "overall" &
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
    c("overall", "age_group and sex")))
  expect_true(all(result %>%
    dplyr::select("strata_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "overall",
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
    c("overall", "age_group and sex")))
  expect_true(all(result %>%
    dplyr::select("group_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "overall",
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
    c("overall")))
  expect_true(any(test_data %>%
    summariseResult(
      strata = list("sex"),
      includeOverallStrata = TRUE
    ) %>%
    dplyr::pull("strata_name") %in%
    c("overall")))

  expect_false(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = FALSE
    ) %>%
    dplyr::pull("group_name") %in%
    c("overall")))
  expect_true(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = TRUE
    ) %>%
    dplyr::pull("group_name") %in%
    c("overall")))

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
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<1") == 0)
  expect_true(sum(is.na(s$estimate)) == 0)

  # minCellCount = 2
  s <- summariseResult(x, minCellCount = 2)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<2") == 4)
  expect_true(sum(is.na(s$estimate)) == 4)

  # minCellCount = 3
  s <- summariseResult(x, minCellCount = 3)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<3") == 8)
  expect_true(sum(is.na(s$estimate)) == 8)

  # minCellCount = 4
  s <- summariseResult(x, minCellCount = 4)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<4") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 5
  s <- summariseResult(x, minCellCount = 5)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<5") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 6
  s <- summariseResult(x, minCellCount = 6)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<6") == 9)
  expect_true(sum(is.na(s$estimate)) == 9)

  # minCellCount = 7
  s <- summariseResult(x, minCellCount = 7)
  expect_true(nrow(s) == 29)
  expect_true(sum(s$estimate[!is.na(s$estimate)] == "<7") == 1)
  expect_true(sum(is.na(s$estimate)) == 28)
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

test_that("test summary table naming", {


  cdm <- PatientProfiles::mockPatientProfiles(
    connectionDetails
  )

  dat <-
    cdm$cohort1 %>% addDemographics() %>%
    dplyr::mutate(age_age = age,
                  age_age_age = age,
                 age_age_age_age = age) %>%
    summariseResult()

  expect_true(all(
    c("age_age", "age", "age_age_age", "age_age_age_age") %in% dat$variable
  ))





})

test_that("misisng counts", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(NA, 40, NA, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(NA, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(connectionDetails$write_schema, "test_table")
  DBI::dbWriteTable(connectionDetails$con, name = name, value = cohort)
  cohort <- dplyr::tbl(connectionDetails$con, name)
  variables <- list(
    numeric = c(
      "age", "number_visits", "prior_history"
    ),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75", "missing"),
    categorical = c("count", "percentage")
  )
  expect_no_error(
    result <- summariseResult(
      cohort, strata = list("sex"), variables = variables,
      functions = functions, minCellCount = 1
    )
  )
  expected <- dplyr::tribble(
    ~strata, ~variable, ~count, ~percentage,
    "overall", "age", 2, 50,
    "overall", "number_visits", 1, 25,
    "overall", "prior_history", 0, 0,
    "Male", "age", 1, 100/3,
    "Male", "number_visits", 1, 100/3,
    "Male", "prior_history", 0, 0,
    "Female", "age", 1, 100,
    "Female", "number_visits", 0, 0,
    "Female", "prior_history", 0, 0,
  ) %>%
    dplyr::mutate(
      count = as.character(.data$count),
      percentage = as.character(.data$percentage)
    )
  for (k in seq_len(nrow(expected))) {
    x <- result %>%
      dplyr::filter(
        .data$strata_level == .env$expected$strata[k],
        .data$variable == .env$expected$variable[k]
      )
    xcount <- x$estimate[x$estimate_type == "count_missing"]
    xpercentage <- x$estimate[x$estimate_type == "percentage_missing"]
    expect_true(xcount == expected$count[k])
    expect_true(xpercentage == expected$percentage[k])
  }
  # female age is all na
  expect_true(
    result %>%
      dplyr::filter(
        .data$variable == "age",
        .data$strata_level == "Female",
        is.na(.data$variable_level),
        !.data$estimate_type %in% c("count_missing", "percentage_missing")
      ) %>%
      dplyr::pull("estimate") %>%
      is.na() %>%
      all()
  )
  DBI::dbRemoveTable(connectionDetails$con, name = name)
})

test_that("data is ordered", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(15, 40, 20, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(5, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(connectionDetails$write_schema, "test_table")
  DBI::dbWriteTable(connectionDetails$con, name = name, value = cohort)
  testTable <- dplyr::tbl(connectionDetails$con, name)
  variables <- list(
    numeric = c("age", "number_visits", "prior_history"),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "percentage", "median")
  )
  expect_no_error(
    result <- summariseResult(
      table = testTable, strata = list("sex"), variables = variables,
      functions = functions, minCellCount = 1
    )
  )
  # check first overall, second sex
  order <- unique(result$strata_level)
  expect_identical(order, c("overall", "Female", "Male"))
  # first numbers, age, sex, prior_history, number_visits
  variables <- unique(result$variable)
  expect_identical(variables, c(
    "number subjects", "number records", "age", "sex", "prior_history",
    "number_visits"
  ))
  # variable levels appear by order
  order <- unique(result$variable_level[result$variable == "sex"])
  order <- order[!is.na(order)]
  expect_identical(order, c("Female", "Male"))
  DBI::dbRemoveTable(connectionDetails$con, name = name)

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(15, 40, 20, 7),
    sex = c("Male", "Male", "xFemale", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(5, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(connectionDetails$write_schema, "test_table")
  DBI::dbWriteTable(connectionDetails$con, name = name, value = cohort)
  testTable <- dplyr::tbl(connectionDetails$con, name)
  variables <- list(
    numeric = c("age", "number_visits", "prior_history"),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "percentage", "median")
  )
  expect_no_error(
    result <- summariseResult(
      table = testTable, strata = list("sex"), variables = variables,
      functions = functions, minCellCount = 1
    )
  )
  # check first overall, second sex
  order <- unique(result$strata_level)
  expect_identical(order, c("overall", "Male", "xFemale"))
  # first numbers, age, sex, prior_history, number_visits
  variables <- unique(result$variable)
  expect_identical(variables, c(
    "number subjects", "number records", "age", "sex", "prior_history",
    "number_visits"
  ))
  # variable levels appear by order
  order <- unique(result$variable_level[result$variable == "sex"])
  order <- order[!is.na(order)]
  expect_identical(order, c("Male", "xFemale"))
  DBI::dbRemoveTable(connectionDetails$con, name = name)
})
