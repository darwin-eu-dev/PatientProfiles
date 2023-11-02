test_that("test summariseCharacteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24)
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3), person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(
      c("1990-01-01", "2005-12-30", "1990-05-12")
    ), observation_period_end_date = as.Date(
      c("2022-01-01", "2022-12-30", "2022-05-12")
    )
  )
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
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
    ))
  )

  cdm <- mockPatientProfiles(
    connectionDetails,
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period
  )

  attr(cdm$dus_cohort, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
  )
  attr(cdm$comorbidities, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
  )
  attr(cdm$medication, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      ), "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    ),
    minCellCount = 1
  ))
  expect_identical(colnames(result), c(
    "cdm_name", "result_type", "group_name", "group_level", "strata_name",
    "strata_level", "variable", "variable_level", "variable_type",
    "estimate_type", "estimate"
  ))
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Exposed") %>%
      dplyr::filter(variable_level == "Covid") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Exposed") %>%
      dplyr::filter(variable_level == "Headache") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Exposed") %>%
      dplyr::filter(variable_level == "Acetaminophen") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    2
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Exposed") %>%
      dplyr::filter(variable_level == "Ibuprophen") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Exposed") %>%
      dplyr::filter(variable_level == "Naloxone") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Unexposed") %>%
      dplyr::filter(variable_level == "Covid") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Unexposed") %>%
      dplyr::filter(variable_level == "Headache") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    1
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Unexposed") %>%
      dplyr::filter(variable_level == "Acetaminophen") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Unexposed") %>%
      dplyr::filter(variable_level == "Ibuprophen") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )
  expect_identical(
    result %>%
      dplyr::filter(group_level == "Unexposed") %>%
      dplyr::filter(variable_level == "Naloxone") %>%
      dplyr::filter(estimate_type == "count") %>%
      dplyr::pull("estimate") %>%
      as.numeric(),
    0
  )

  expect_no_error(result <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = list(
          "short" = c(-30, 0), "long" = c(-365, 0)
        )
      ), "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    ), minCellCount = 1
  ))
  expect_identical(colnames(result), c(
    "cdm_name", "result_type", "group_name", "group_level", "strata_name",
    "strata_level", "variable", "variable_level", "variable_type",
    "estimate_type", "estimate"
  ))
  expect_true(
    result %>%
      dplyr::filter(grepl("short", variable)) %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      attr(cdm$medication, "cohort_set") %>%
        dplyr::tally() * 8 # 2 group_level 4 estimate type
  )
  expect_true(
    result %>%
      dplyr::filter(grepl("long", variable)) %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      attr(cdm$medication, "cohort_set") %>%
        dplyr::tally() * 8 # 2 group_level 4 estimate type
  )
  expect_true(
    result %>%
      dplyr::filter(grepl("Medications", variable)) %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      attr(cdm$medication, "cohort_set") %>%
        dplyr::tally() * 16 # 2 group_level 4 estimate type 2 window
  )
  expect_true(
    result %>%
      dplyr::filter(grepl("Comorbidities", variable)) %>%
      dplyr::tally() %>%
      dplyr::pull() ==
      attr(cdm$comorbidities, "cohort_set") %>%
        dplyr::tally() * 8 # 2 group_level 4 estimate type
  )

  result_notables <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(), tableIntersect = list(), minCellCount = 1
  )
  expect_true(
    all(c("cdm_name", "result_type", "group_name", "group_level", "strata_name", "strata_level", "variable", "variable_level", "variable_type", "estimate_type", "estimate") %in%
      colnames(result_notables))
  )
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
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )


  cdm <- mockPatientProfiles(
    connectionDetails,
    dus_cohort = dus_cohort,
    comorbidities = comorbidities, medication = medication
  )

  expect_no_error(
    cdm$dus_cohort %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseCharacteristics(cohortIntersect = list(
        "Medications" = list(
          targetCohortTable = "medication", value = "flag", window = c(-365, 0)
        ), "Comorbidities" = list(
          targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
        )
      ), minCellCount = 1)
  )
})
