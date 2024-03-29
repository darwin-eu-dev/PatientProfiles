test_that("plotCohortTiming, boxplot", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=2000),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=28),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10,
                   6, 18, 5, 1, 20, 14, 13, 8, 17, 3,
                   16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18,
                   5, 12, 3, 14, 13),
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
  boxplot1 <- plotCohortTiming(timing1,
                               facetBy = "cdm_name",
                               color = c("cohort_name_reference", "cohort_name_comparator"),
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = TRUE)
  expect_true(all(c("q0", "q25", "q50", "q75", "q100") %in% colnames(boxplot1$data)))
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot1$data$cohort_name_reference))
  expect_true(all(c("Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot1$data$cohort_name_comparator))
  expect_false("Cohort 1" %in% boxplot1$data$cohort_name_comparator)
  expect_true(all(c("gg", "ggplot") %in% class(boxplot1)))
  expect_true(boxplot1$labels$fill == "group")

  boxplot2 <- plotCohortTiming(timing1,
                               color = NULL,
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot2$data$cohort_name_reference))
  expect_true(all(c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot2$data$cohort_name_comparator))
  expect_true(all(c("gg", "ggplot") %in% class(boxplot2)))

  # strata
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing3 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")))
  boxplot3 <- plotCohortTiming(timing3,
                               color = "strata_level",
                               facetBy = "strata_name",
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot3$data$cohort_name_reference))
  expect_true(all(c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot3$data$cohort_name_comparator))
  expect_true(all(c("gg", "ggplot") %in% class(boxplot3)))
  expect_true(boxplot3$labels$fill == "group")
  expect_true(all(c("Overall", "0 to 40", "0 to 40 and female", "41 to 150", "41 to 150 and female") %in% unique(boxplot3$data$group)))

  CDMConnector::cdm_disconnect(cdm)
})
test_that("plotCohortTiming, density", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=2000),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=28),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10,
                   6, 18, 5, 1, 20, 14, 13, 8, 17, 3,
                   16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18,
                   5, 12, 3, 14, 13),
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
                                   density = TRUE)
  density1 <- plotCohortTiming(timing1,
                               type = "density",
                               facetBy = NULL,
                               color = c("cohort_name_reference"),
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = TRUE)

  expect_true(all(c("plot_id", "timing_label", "color_var", "x", "y", ".group") %in% colnames(density1$data)))
  expect_true(all(c("gg", "ggplot") %in% class(density1)))
  expect_true(density1$labels$fill == "color_var")

  density2 <- plotCohortTiming(timing1,
                               type = "density",
                               color = NULL,
                               facetBy = "cdm_name",
                               timingLabel = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("plot_id", "timing_label", "x", "y", ".group") %in% colnames(density2$data)))
  expect_true(all(c("gg", "ggplot") %in% class(density2)))
  expect_null(density2$labels$fill)

  timing2 <- summariseCohortTiming(cdm$table,
                                   timing = character(),
                                   density = TRUE)
  density4 <- plotCohortTiming(timing2,
                               type = "density",
                               facetBy = NULL,
                               color = c("cohort_name_reference"),
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = TRUE)
  expect_true(all(c("gg", "ggplot") %in% class(density4)))
  expect_true(all(is.na(density4$data$q50)))

  # strata
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing3 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")),
                                   density = TRUE)

  density3 <- plotCohortTiming(timing3,
                               type = "density",
                               color = "strata_level",
                               facetBy = "strata_name",
                               timingLabel = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("plot_id", "timing_label", "color_var", "x", "y", ".group") %in% colnames(density3$data)))
  expect_true(all(c("gg", "ggplot") %in% class(density3)))
  expect_true(all(unique(density3$data$color_var) %in% c("Overall", "0 to 40", "0 to 40 and female",
                                                         "41 to 150", "41 to 150 and female", "41 to 150 and male",
                                                         "0 to 40 and male")))

  CDMConnector::cdm_disconnect(cdm)
})
test_that("plotCohortOverlap", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=1980),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=28),
    race_concept_id= 0,
    ethnicity_concept_id = 0
  )

  table <- dplyr::tibble(
    cohort_definition_id = c(rep(1, 15), rep(2, 10), rep(3, 15), rep(4, 5)),
    subject_id = c(20, 5, 10, 12, 4, 15, 2, 1, 5, 10, 5, 8, 13, 4, 10,
                   6, 18, 5, 1, 20, 14, 13, 8, 17, 3,
                   16, 15, 20, 17, 3, 14, 6, 11, 8, 7, 20, 19, 5, 2, 18,
                   5, 12, 3, 14, 13),
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

  overlap <- summariseCohortOverlap(cdm$table)

  gg1 <- plotCohortOverlap(overlap,
                           overlapLabel = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}")
  expect_true("ggplot" %in% class(gg1))
  expect_false("cohort_4" %in% gg1$data$cohort_name_reference)


  gg2 <- plotCohortOverlap(overlap |> dplyr::filter(.data$variable_name == "number_subjects",
                                                    .data$estimate_name == "percentage"),
                           facetBy = "cdm_name",
                           uniqueCombinations = TRUE)
  expect_true("ggplot" %in% class(gg2))
  expect_true(gg2$data |> dplyr::filter(variable_name == "number subjects") |> nrow() == 0)
  expect_true(nrow(gg2$data |>
                     dplyr::filter(.data$cohort_name_reference %in% c("cohort_1", "cohort_2") &
                                     .data$cohort_name_comparator %in% c("cohort_1", "cohort_2"))) == 3)
  # strata ----
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0,40), c(41,150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  overlap2 <- summariseCohortOverlap(cdm$table,
                                     strata = list("age_group", c("age_group", "sex")))
  gg3 <- plotCohortOverlap(overlap2 |> dplyr::filter(.data$variable_name == "number_subjects"),
                           facetBy = c("strata_name", "strata_level"),
                           overlapLabel = "{cohort_name_reference}_{cohort_name_comparator}",
                           uniqueCombinations = FALSE)
  expect_true("ggplot" %in% class(gg3))

  # > 1 CDM
  overlap3 <- overlap |>
    dplyr::union_all(
      overlap |>
        dplyr::mutate(cdm_name = "cdm2") |>
        dplyr::filter(.data$group_level != "cohort_2 &&& cohort_4")) |>
    dplyr::filter(.data$variable_name == "number_subjects")
  gg4 <- plotCohortOverlap(overlap3,
                           facetBy = "cdm_name",
                           uniqueCombinations = FALSE)
  # expect_true(nrow(gg4$data |> dplyr::distinct(comparison_name, y_pos)) == 12)

  CDMConnector::cdm_disconnect(cdm)
})
test_that("plotTableIntersect", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
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
  visit_ocurrence <- dplyr::tibble(
    visit_occurrence_id = 1:4,
    person_id = c(1, 1, 2, 3),
    visit_concept_id = NA_character_,
    visit_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    visit_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    visit_type_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1975-01-01", "1959-04-29", "1944-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2021-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    dus_cohort = dus_cohort, person = person,
    observation_period = observation_period,
    visit_occurrence = visit_ocurrence
  )

  result1 <- summariseCharacteristics(
    cdm$dus_cohort,
    tableIntersect = list(
      "Visit history" = list(
        tableName = "visit_occurrence", value = "count", window = c(-Inf, 0)
      )
    )
  )

  gg1 <- plotTableIntersect(result1)
  expect_true(ggplot2::is.ggplot(gg1))
  expect_true(unique(gg1$data$result_type) == "summarised_table_intersect")

  CDMConnector::cdm_disconnect(cdm)
})
test_that("plotCohortIntersect", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
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
      "1975-01-01", "1959-04-29", "1944-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2021-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    dus_cohort = dus_cohort, person = person,
    comorbidities = comorbidities, medication = medication,
    observation_period = observation_period
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

  result1 <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      ),
      "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    )
  )

  result1 <- result1 %>% dplyr::filter(estimate_name == "percentage")
  gg1 <- plotCohortIntersect(result1)
  expect_true(ggplot2::is.ggplot(gg1))
  expect_true(unique(gg1$data$result_type) == "summarised_cohort_intersect")

  CDMConnector::cdm_disconnect(cdm)
})
test_that("plotDemographics", {
  person <- dplyr::tibble(
    person_id = c(1, 2, 3), gender_concept_id = c(8507, 8532, 8532),
    year_of_birth = c(1985, 2000, 1962), month_of_birth = c(10, 5, 9),
    day_of_birth = c(30, 10, 24),
    race_concept_id = 0,
    ethnicity_concept_id = 0
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
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1975-01-01", "1959-04-29", "1944-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2021-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    dus_cohort = dus_cohort, person = person,
    observation_period = observation_period
  )

  result1 <- summariseCharacteristics(
    cdm$dus_cohort,
    demographics = TRUE,
    ageGroup = list(c(0, 40), c(41, 150))
  )

  gg1 <- plotDemographics(result1)
  expect_true(ggplot2::is.ggplot(gg1))
  expect_true(unique(gg1$data$result_type) == "summarised_demographics")

  gg2 <- plotDemographics(result1, plotStyle = "boxplot", colorVars = "variable_name")
  expect_true(ggplot2::is.ggplot(gg2))
  expect_true(unique(gg2$data$result_type) == "summarised_demographics")

  CDMConnector::cdm_disconnect(cdm)
})
