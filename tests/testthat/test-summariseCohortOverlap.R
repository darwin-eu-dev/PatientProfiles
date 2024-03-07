test_that("summariseCohortOverlap", {
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

  overlap1 <- summariseCohortOverlap(cdm$table)
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap1))
  expect_equal(overlap1$group_name |> unique(),
               "cohort_name_reference and cohort_name_comparator")
  expect_true(nrow(overlap1) == 32)
  expect_equal(cdm$table |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::distinct(subject_id) |>
                 dplyr::tally() |> dplyr::pull(),
               as.numeric(overlap1$estimate_value[
                 overlap1$variable_name == "number subjects" &
                   overlap1$group_level == "cohort_1 and cohort_1"
               ])
  )
  expect_equal(cdm$table |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::tally() |> dplyr::pull(),
               as.numeric(overlap1$estimate_value[
                 overlap1$variable_name == "number records" &
                   overlap1$group_level == "cohort_1 and cohort_1"
               ])
  )

  CDMConnector::cdm_disconnect(cdm)
})

test_that("tableCohortOverlap", {
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

  overlap <- summariseCohortOverlap(cdm$table)

  gtResult1 <- tableCohortOverlap(overlap, cohortNameReference = "cohort_1")
  expect_true("gt_tbl" %in% class(gtResult1))
  expect_equal(gtResult1$`_data`$`Cdm name`,
               c("PP_MOCK", rep("", nrow(gtResult1$`_data`)-1)))
  expect_equal(unique(gtResult1$`_data`$`Cohort name reference`)[1], "cohort_1")
  expect_equal(unique(gtResult1$`_data`$`Cohort name comparator`),
               c("cohort_2", "", "cohort_3", "cohort_4"))

  fxResult1 <- tableCohortOverlap(overlap,
                                  type = "flextable",
                                  minCellCount = 1000,
                                  variableName = "number records")
  expect_true("flextable" %in% class(fxResult1))
  expect_true(all(grepl("<1,000", fxResult1$body$dataset$Overlap)))
  expect_false("number subjects" %in% fxResult1$body$dataset$`Variable name`)

  tibbleResult1 <-  tableCohortOverlap(overlap, type = "tibble")
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(tibbleResult1)))

  expect_warning(
    tibbleResult2 <-  tableCohortOverlap(overlap, type = "tibble", cohortNameReference = c("hola"))
  )
  expect_true(nrow(tibbleResult2) == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("plotCohortOverlap", {
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

  overlap <- summariseCohortOverlap(cdm$table)

  gg1 <- plotCohortOverlap(overlap,
                           overlapLabel = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}")
  expect_true("ggplot" %in% class(gg1))
  expect_true(gg1$data$total |> unique() == 100)
  expect_true(gg1$data |> dplyr::filter(variable_name == "number records") |> nrow() == 0)
  expect_false("cohort_4" %in% gg1$data$cohort_name_reference)


  gg2 <- plotCohortOverlap(overlap,
                           cohortNameReference = c("cohort_1", "cohort_2"),
                           variableName = "number records",
                           facetBy = "cdm_name",
                           uniqueCombinations = FALSE)
  expect_true("ggplot" %in% class(gg2))
  expect_true(gg2$data |> dplyr::filter(variable_name == "number subjects") |> nrow() == 0)
  expect_true(gg2$data$facet_var |> unique() == "PP_MOCK")
  expect_true(nrow(gg2$data |>
                dplyr::filter(.data$cohort_name_reference %in% c("cohort_1", "cohort_2") &
                                .data$cohort_name_comparator %in% c("cohort_1", "cohort_2"))) == 2)
  CDMConnector::cdm_disconnect(cdm)
})
