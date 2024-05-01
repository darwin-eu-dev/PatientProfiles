test_that("test summariseCharacteristics", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4), gender_concept_id = c(8507, 8532, 8532, 8507),
    year_of_birth = c(1985, 2000, 1962, 1999), month_of_birth = c(10, 5, 9,4),
    day_of_birth = c(30, 10, 24, 20),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2, 2),
    subject_id = c(1, 1, 2, 4, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1, 1),
    subject_id = c(1, 1, 3, 4, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1, 2),
    subject_id = c(1, 1, 2, 3, 4),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01", "2009-01-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01", "2009-01-01"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 2, 3, 4),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03", "2000-01-01"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    connectionDetails,
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period, cohort1 = emptyCohort,
    cohort2 = emptyCohort
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    ))
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
    ))
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )
  result <- summariseCharacteristics(
  cdm$dus_cohort,
  cohortIntersect = list(
    "Medications" = list(
      targetCohortTable = "medication", value = "flag", window = c(-365, 0)
    ), "Comorbidities" = list(
      targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
    )
  )
)
  expect_true(all(compute_smd(result)$variable_name %in% result$variable_name))
  }
)
