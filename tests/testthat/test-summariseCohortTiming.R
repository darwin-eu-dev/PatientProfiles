test_that("summariseCohortTiming", {
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

  expect_true(all(c("min", "q25","median","q75","max","count", "restrict_to_first_entry") %in%
                    timing1$estimate_name |> unique()))
  expect_equal(timing1$estimate_value[1], timing1$estimate_value[2])
  expect_true(omopgenerics::settings(timing1)$restrict_to_first_entry)

  timing2 <- summariseCohortTiming(cdm$table,
                                    restrictToFirstEntry = FALSE,
                                    timing = c("min",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing2))
  expect_false(timing2$estimate_value[5] == timing2$estimate_value[6])
  expect_true(all(timing2$estimate_name |> unique() %in%
                    c("min","max","count", "restrict_to_first_entry")))

  timing3 <- summariseCohortTiming(cdm$table,
                                   restrictToFirstEntry = FALSE,
                                   timing = character(),
                                   density = TRUE)
  expect_true(all(c("density", "settings") %in%
                    unique(timing3$variable_name)))
  expect_true(all(c("x", "y") %in%
                    unique(timing3$estimate_name)))
  expect_true("overall" == unique(timing3$strata_level))

  ## Strata and cohortId----
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0,40), c(41, 150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing4 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")))
  expect_true(all(c("overall", "age_group", "age_group &&& sex") %in%
                    unique(timing4$strata_name)))

  # add density tests
  timing5 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")),
                                   timing = character(),
                                   density = TRUE)
  timing5 <- timing5 |> dplyr::filter(.data$variable_name != "settings")
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "age_group &&& sex"]) %in% c("x", "y", "count")))
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "overall"]) %in% c("x", "y", "count")))
  expect_true(all(unique(timing5$estimate_name[timing5$strata_name == "age_group"]) %in% c("x", "y", "count")))

  timing6 <- summariseCohortTiming(cdm$table, cohortId = 1)
  expect_true(nrow(timing6) == 0)

  expect_warning(timing7 <- summariseCohortTiming(cdm$table,
                                                  cohortId = 5:7))
  expect_true(nrow(timing7) == 0)

  timing8 <- summariseCohortTiming(cdm$table, cohortId = 1, density = TRUE)
  expect_true(nrow(timing8) == 0)

  CDMConnector::cdm_disconnect(cdm)

})

test_that("tableCohortTiming", {
  person <- dplyr::tibble(
    person_id = 1:20,
    gender_concept_id = 8532,
    year_of_birth = runif(n=20, min=1950, max=1970),
    month_of_birth = runif(n=20, min=1, max=12),
    day_of_birth = runif(n=20, min=1, max=28),
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
  tibble1 <- tableCohortTiming(timing1, type = "tibble", header = c("Strata", "strata_name", "strata_level"), splitStrata = FALSE)
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator",
                    "Variable name", "Estimate name", "[header]Strata\n[header_level]Overall\n[header_level]Overall") %in%
                    colnames(tibble1)))
  expect_true(nrow(tibble1 |> dplyr::distinct(`Cohort name reference`, `Cohort name comparator`)) == 6)
  expect_true(all(unique(tibble1$`Estimate name`) %in% c("N", "Median [Q25 - Q75]", "Range")))

  tibble2 <- tableCohortTiming(timing1, type = "tibble", cohortNameReference = c("cohort_1"))
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator",
                    "Variable name", "Estimate name", "Estimate value") %in%
                    colnames(tibble2)))
  expect_true("Cohort 1" == unique(tibble2$`Cohort name reference`))
  expect_false(all(unique(tibble2$`Cohort name comparator`) %in%
                     unique(tibble2$`Cohort name reference`)))

  tibble3 <- tableCohortTiming(timing1, type = "tibble", .options = list(uniqueCombinations = FALSE))
  expect_true(all(unique(tibble3$`Cohort name comparator`) %in%
                    unique(tibble3$`Cohort name reference`)))

  gt1 <- tableCohortTiming(timing1, type = "gt",
                           cohortNameComparator = "cohort_1")
  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(gt1$`_data`$`Cohort name comparator` |> unique() %in% c("Cohort 1", "")))

  # strata ----
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing3 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")))

  fx1 <- tableCohortTiming(timing3, type = "flextable", cohortNameReference = "cohort_1",
                           strataName = NULL,
                           strataLevel = NULL,
                           splitStrata = FALSE,
                           header = c("Strata", "strata_name", "strata_level"),
                           minCellCount = 5)
  expect_true(class(fx1) == "flextable")
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator",
                    "Variable name", "Estimate name", "Strata\nOverall\nOverall",
                    "Strata\nAge group\n0 to 40", "Strata\nAge group\n41 to 150", "Strata\nAge group and sex\n0 to 40 and female",
                    "Strata\nAge group and sex\n41 to 150 and female") %in%
                    colnames(fx1$body$dataset)))

  CDMConnector::cdm_disconnect(cdm)
})

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
  boxplot1 <- plotCohortTiming(timing1,
                                cohortNameReference = c("cohort_1", "cohort_2"),
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
                                cohortNameReference = c("cohort_1", "cohort_2"),
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
                               cohortNameReference = c("cohort_1", "cohort_2"),
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
                                   density = TRUE)
  density1 <- plotCohortTiming(timing1,
                               type = "density",
                               cohortNameReference = c("cohort_1", "cohort_2"),
                               facetBy = NULL,
                               color = c("cohort_name_reference"),
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = TRUE)

  expect_true(all(c("q0", "q25", "q50", "q75", "q100") %in% colnames(density1$data)))
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot1$data$cohort_name_reference))
  expect_true(all(c("Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot1$data$cohort_name_comparator))
  expect_false("Cohort 1" %in% boxplot1$data$cohort_name_comparator)
  expect_true(all(c("gg", "ggplot") %in% class(boxplot1)))
  expect_true(boxplot1$labels$fill == "group")
  expect_true(unique(boxplot1$data$facet_var) == "PP_MOCK")

  boxplot2 <- plotCohortTiming(timing1,
                               cohortNameReference = c("cohort_1", "cohort_2"),
                               color = NULL,
                               timingLabel = "{cohort_name_reference}; {cohort_name_comparator}",
                               uniqueCombinations = FALSE)
  expect_true(all(c("Cohort 1", "Cohort 2") %in% boxplot2$data$cohort_name_reference))
  expect_true(all(c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4") %in% boxplot2$data$cohort_name_comparator))
  expect_true(all(c("gg", "ggplot") %in% class(boxplot2)))
  expect_false(any(c("facet_var", "group") %in% colnames(boxplot2$data)))

  CDMConnector::cdm_disconnect(cdm)
})
