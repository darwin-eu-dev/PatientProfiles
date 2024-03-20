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
  expect_true(unique(boxplot1$data$facet_var) == "PP_MOCK")

  boxplot2 <- plotCohortTiming(timing1,
                               color = NULL,
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot2$data$cohort_name_reference))
  expect_true(all(c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot2$data$cohort_name_comparator))
  expect_true(all(c("gg", "ggplot") %in% class(boxplot2)))
  expect_false(any(c("facet_var", "group") %in% colnames(boxplot2$data)))

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
  expect_true(all(c("Overall", "Age group", "Age group and sex") %in% unique(boxplot3$data$facet_var)))
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
  expect_false("facet_var" %in% colnames(density1$data))

  density2 <- plotCohortTiming(timing1,
                               type = "density",
                               color = NULL,
                               facetBy = "cdm_name",
                               timingLabel = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("plot_id", "timing_label", "facet_var", "x", "y", ".group") %in% colnames(density2$data)))
  expect_true(all(c("gg", "ggplot") %in% class(density2)))
  expect_null(density2$labels$fill)
  expect_true("facet_var" %in% colnames(density2$data))
  expect_true(unique(density2$data$facet_var) == "PP_MOCK")

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
  expect_true(all(c("plot_id", "timing_label", "color_var", "facet_var", "x", "y", ".group") %in% colnames(density3$data)))
  expect_true(all(c("gg", "ggplot") %in% class(density3)))
  expect_true(all(c("Overall", "Age group", "Age group and sex") %in% unique(density3$data$facet_var)))
  expect_true(all(unique(density3$data$color_var) %in% c("Overall", "0 to 40", "0 to 40 and female",
                                                         "41 to 150", "41 to 150 and female", "41 to 150 and male",
                                                         "0 to 40 and male")))

  CDMConnector::cdm_disconnect(cdm)
})