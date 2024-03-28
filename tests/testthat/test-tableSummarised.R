test_that("tableCharacteristics", {
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


  expect_no_error(gt1 <- tableCharacteristics(result1, excludeColumns = c("result_id", "result_type",
                                                         "package_name", "package_version",
                                                         "estimate_type", "additional_name",
                                                         "additional_level", "cdm_name")))
  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(c("Variable name", "Variable level", "Estimate name") %in%
                    colnames(gt1$`_data`)))

  fx1 <- tableCharacteristics(result1, header = c("cdm_name", "group", "strata"), type = "flextable")
  expect_true(class(fx1) == "flextable")
  expect_true(all(c("Variable name", "Variable level", "Estimate name",
                    "CDM name\nPP_MOCK\nCohort name\nExposed", "CDM name\nPP_MOCK\nCohort name\nUnexposed") %in%
                    colnames(fx1$body$dataset)))
  expect_true(all(fx1$body$dataset$`Variable name` %>% unique() %in%
                    c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Age", "Sex", "Prior observation",
                      "Future observation", "Medications", "Comorbidities")))

  tibble1 <- tableCharacteristics(result1, type = "tibble", split = "strata", header = character())
  expect_true(all(class(tibble1) %in% c("tbl_df", "tbl", "data.frame")))
  expect_true(all(c("Variable name", "Variable level", "Estimate name",
                    "CDM name", "Group name", "Group level", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(nrow(tibble1) == 53)
})

test_that("tableCohortOverlap", {
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

  overlap <- summariseCohortOverlap(cdm$table)

  gtResult1 <- tableCohortOverlap(overlap)
  expect_true("gt_tbl" %in% class(gtResult1))
  expect_equal(gtResult1$`_data`$`CDM name`,
               c("PP_MOCK", rep("", nrow(gtResult1$`_data`)-1)))
  expect_equal(unique(gtResult1$`_data`$`Cohort name reference`)[1], "Cohort 1")
  expect_true(all(c("Cohort 2", "Cohort 3", "Cohort 4") %in%
                    unique(gtResult1$`_data`$`Cohort name comparator`)))
  expect_false(any(c("Strata name", "Strata level") %in% colnames(gtResult1$`_data`)))

  fxResult1 <- tableCohortOverlap(overlap,
                                  type = "flextable",
                                  split = character(),
                                  header = "group",
                                  excludeColumns = c("result_id", "result_type",
                                                     "package_name", "package_version",
                                                     "estimate_type", "strata_name",
                                                     "strata_level", "additional_name",
                                                     "additional_level"))
  expect_true("flextable" %in% class(fxResult1))
  expect_true(all(c("CDM name",
                    "Estimate name",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 2\nNumber subjects\nOnly in reference",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 2\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 2\nNumber subjects\nOverlap",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 3\nNumber subjects\nOnly in reference",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 3\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 3\nNumber subjects\nOverlap",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 4\nNumber subjects\nOnly in reference",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 4\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 1 and cohort 4\nNumber subjects\nOverlap",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 3\nNumber subjects\nOnly in reference",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 3\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 3\nNumber subjects\nOverlap",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 4\nNumber subjects\nOnly in reference",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 4\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 2 and cohort 4\nNumber subjects\nOverlap",
                    "Cohort name reference and cohort name comparator\nCohort 3 and cohort 4\nNumber subjects\nOnly in reference" ,
                    "Cohort name reference and cohort name comparator\nCohort 3 and cohort 4\nNumber subjects\nOnly in comparator",
                    "Cohort name reference and cohort name comparator\nCohort 3 and cohort 4\nNumber subjects\nOverlap") %in%
                    colnames(fxResult1$body$dataset)))

  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0,40), c(41,100))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()

  overlap3 <- summariseCohortOverlap(cdm$table,
                                     strata = list("age_group", c("age_group", "sex")))
  tibbleResult1 <-  tableCohortOverlap(overlap3,
                                       type = "tibble",
                                       split = character(),
                                       header = character())
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(tibbleResult1)))
  expect_true(all(c("CDM name", "Group name", "Group level", "Strata name", "Strata level", "Estimate name",
                    "Additional name", "Additional level",
                    "[header_level]Number subjects\n[header_level]Only in reference",
                    "[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header_level]Number subjects\n[header_level]Overlap" ) %in% colnames(tibbleResult1)))

  gtResult2 <-  tableCohortOverlap(overlap3,
                                   type = "gt")
  expect_true("gt_tbl" %in% class(gtResult2))
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator", "Estimate name",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in reference",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Overlap",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in reference",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Overlap",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Only in reference",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header]Age group\n[header_level]0 to 40\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Overlap",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Only in reference",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header]Age group\n[header_level]41 to 100\n[header]Sex\n[header_level]Female\n[header_level]Number subjects\n[header_level]Overlap",
                    "[header]Age group\n[header_level]Overall\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in reference",
                    "[header]Age group\n[header_level]Overall\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Only in comparator",
                    "[header]Age group\n[header_level]Overall\n[header]Sex\n[header_level]Overall\n[header_level]Number subjects\n[header_level]Overlap" ) %in%
                    colnames(gtResult2$`_data`)))

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
  tibble1 <- tableCohortTiming(timing1, type = "tibble", header = c("strata"), split = c("group", "additional"))
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator",
                    "Variable name", "Estimate name", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(nrow(tibble1 |> dplyr::distinct(`Cohort name reference`, `Cohort name comparator`)) == 6)
  expect_true(all(unique(tibble1$`Estimate name`) %in% c("N", "Median [Q25 - Q75]", "Range")))

  tibble2 <- tableCohortTiming(timing1, type = "tibble", header = "group")
  expect_true(all(c("CDM name", "Variable name", "Estimate name",
                    "[header]Cohort name reference\n[header_level]Cohort 1\n[header]Cohort name comparator\n[header_level]Cohort 2",
                    "[header]Cohort name reference\n[header_level]Cohort 1\n[header]Cohort name comparator\n[header_level]Cohort 3",
                    "[header]Cohort name reference\n[header_level]Cohort 1\n[header]Cohort name comparator\n[header_level]Cohort 4",
                    "[header]Cohort name reference\n[header_level]Cohort 2\n[header]Cohort name comparator\n[header_level]Cohort 3",
                    "[header]Cohort name reference\n[header_level]Cohort 2\n[header]Cohort name comparator\n[header_level]Cohort 4",
                    "[header]Cohort name reference\n[header_level]Cohort 3\n[header]Cohort name comparator\n[header_level]Cohort 4") %in%
                    colnames(tibble2)))

  tibble3 <- tableCohortTiming(timing1, type = "tibble", .options = list(uniqueCombinations = FALSE))
  expect_true(all(unique(tibble3$`Cohort name comparator`) %in%
                    unique(tibble3$`Cohort name reference`)))

  tibble4 <- tableCohortTiming(timing1, type = "tibble", header = "group", split = character())
  expect_true(all(c("CDM name", "Strata name", "Strata level", "Variable name",
                    "Estimate name", "Additional name", "Additional level",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 2",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 3",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 4",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 2 and cohort 3",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 2 and cohort 4",
                    "[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 3 and cohort 4") %in%
                    colnames(tibble4)))

  gt1 <- tableCohortTiming(timing1, type = "gt")
  expect_true("gt_tbl" %in% class(gt1))

  fx1 <- tableCohortTiming(timing1, type = "flextable")
  expect_true("flextable" %in% class(fx1))

  # strata ----
  cdm$table <- cdm$table |>
    addAge(ageGroup = list(c(0, 40), c(41, 150))) |>
    addSex() |>
    dplyr::compute(name = "table", temporary = FALSE) |>
    omopgenerics::newCohortTable()
  timing2 <- summariseCohortTiming(cdm$table,
                                   strata = list("age_group", c("age_group", "sex")))

  fx2 <- tableCohortTiming(timing2,
                           type = "flextable")
  expect_true(class(fx2) == "flextable")
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator", "Variable name",
                    "Estimate name", "Age group\nOverall\nSex\nOverall","Age group\n0 to 40\nSex\nOverall", "Age group\n41 to 150\nSex\nOverall",
                    "Age group\n0 to 40\nSex\nFemale", "Age group\n41 to 150\nSex\nFemale") %in%
                    colnames(fx2$body$dataset)))

  gt2 <- tableCohortTiming(timing2, split = c("group", "additional"))
  expect_true("gt_tbl" %in% class(gt2))
  expect_true(all(c("CDM name", "Cohort name reference", "Cohort name comparator", "Variable name",
                    "Estimate name", "[header_level]Overall\n[header_level]Overall",
                    "[header_level]Age group\n[header_level]0 to 40",
                    "[header_level]Age group\n[header_level]41 to 150",
                    "[header_level]Age group and sex\n[header_level]0 to 40 and female",
                    "[header_level]Age group and sex\n[header_level]41 to 150 and female") %in%
                    colnames(gt2$`_data`)))

  gt3 <- tableCohortTiming(timing2 |> dplyr::filter(grepl("cohort_1", group_level)) |> dplyr::filter(grepl("2|3", group_level)),
                           split = c("additional"), header = c("cdm_name", "group", "strata"))
  expect_true("gt_tbl" %in% class(gt3))
  expect_true(all(c("Variable name",
                    "Estimate name",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 2\n[header_level]Age group\n[header_level]41 to 150",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 2\n[header_level]Age group and sex\n[header_level]41 to 150 and female",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 2\n[header_level]Overall\n[header_level]Overall",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 3\n[header_level]Age group\n[header_level]41 to 150",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 3\n[header_level]Age group and sex\n[header_level]41 to 150 and female",
                    "[header]CDM name\n[header_level]PP_MOCK\n[header_level]Cohort name reference and cohort name comparator\n[header_level]Cohort 1 and cohort 3\n[header_level]Overall\n[header_level]Overall") %in%
                    colnames(gt3$`_data`)))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("tableDemographics", {
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

  result1 <- summariseDemographics(
    cdm$dus_cohort,
    ageGroup = list(c(0,40), c(41,150))
  )

  expect_no_error(gt1 <- tableDemographics(result1,
                                           excludeColumns = c("result_id", "result_type",
                                                              "package_name", "package_version",
                                                              "estimate_type", "additional_name",
                                                              "additional_level", "cdm_name")))
  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(c("Variable name", "Variable level", "Estimate name") %in%
                    colnames(gt1$`_data`)))
  expect_true(all(gt1$`_data`$`Variable name` %in%
                     c("Age", "Sex", "Prior observation", "Future observation", "Age group", "")))

  result1 <- summariseCharacteristics(
    cdm$dus_cohort,
    ageGroup = list(c(0,40), c(41,150))
  )
  fx1 <- tableDemographics(result1, header = c("cdm_name", "group", "strata"), type = "flextable")
  expect_true(class(fx1) == "flextable")
  expect_true(all(c("Variable name", "Variable level", "Estimate name",
                    "CDM name\nPP_MOCK\nCohort name\nCohort 2", "CDM name\nPP_MOCK\nCohort name\nCohort 1") %in%
                    colnames(fx1$body$dataset)))
  expect_true(all(fx1$body$dataset$`Variable name` %>% unique() %in%
                    c( "Age", "Sex", "Prior observation",
                      "Future observation", "Age group")))

  tibble1 <- tableDemographics(result1, type = "tibble", split = "strata", header = character())
  expect_true(all(class(tibble1) %in% c("tbl_df", "tbl", "data.frame")))
  expect_true(all(c("Variable name", "Variable level", "Estimate name",
                    "CDM name", "Group name", "Group level", "Estimate value") %in%
                    colnames(tibble1)))
  expect_true(nrow(tibble1) == 29)

  CDMConnector::cdm_disconnect(cdm)
})

 test_that("tableCohortIntersect", {
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

  expect_no_error(gt1 <- tableCohortIntersect(
    result1,
    excludeColumns = c("result_id", "result_type",
                       "package_name", "package_version",
                       "estimate_type", "additional_name",
                       "additional_level", "cdm_name"))
  )
  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(c("Variable name", "Variable level", "Estimate name") %in%
                    colnames(gt1$`_data`)))
  expect_true(all(gt1$`_data`$`Variable name` %in%
                    c("Medications", "Comorbidities", "")))
  expect_true(nrow(gt1$`_data`) == 5)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("tableTabletIntersect", {
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

  expect_no_error(gt1 <- tableTableIntersect(result1))

  expect_true("gt_tbl" %in% class(gt1))
  expect_true(all(colnames(gt1$`_data`) %in%
                    c("CDM name", "Variable name", "Estimate name",
                      "[header]Cohort name\n[header_level]Cohort 1",
                      "[header]Cohort name\n[header_level]Cohort 2")))
  expect_true(all(gt1$`_data`$`Variable name` %in% c("Visit history", "")))

  CDMConnector::cdm_disconnect(cdm)
})
