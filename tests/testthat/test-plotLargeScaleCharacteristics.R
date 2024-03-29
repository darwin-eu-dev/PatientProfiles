# Test 1: Function returns a ggplot object
test_that("Function returns a ggplot object", {
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = c(8507, 8532),
    year_of_birth = c(1990, 1992),
    month_of_birth = c(1, 1),
    day_of_birth = c(1, 1),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2011-10-07", "2000-01-01")),
    observation_period_end_date = as.Date(c("2031-10-07", "2030-01-01")),
    period_type_concept_id = 44814724
  )
  cohort_interest <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    ))
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:11,
    person_id = c(rep(1, 8), rep(2, 3)),
    drug_concept_id = c(
      rep(1125315, 2), rep(1503328, 5), 1516978, 1125315, 1503328, 1516978
    ),
    drug_exposure_start_date = as.Date(c(
      "2010-10-01", "2012-12-31", "2010-01-01", "2012-09-01", "2013-04-01",
      "2014-10-31", "2015-05-01", "2015-10-01", "2012-01-01", "2012-10-01",
      "2014-10-12"
    )),
    drug_exposure_end_date = as.Date(c(
      "2010-12-01", "2013-05-12", "2011-01-01", "2012-10-01", "2013-05-01",
      "2014-12-31", "2015-05-02", "2016-10-01", "2012-01-01", "2012-10-30",
      "2015-01-10"
    )),
    drug_type_concept_id = 38000177,
    quantity = 1
  )
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:8,
    person_id = c(rep(1, 4), rep(2, 4)),
    condition_concept_id = c(
      317009, 378253, 378253, 4266367, 317009, 317009, 378253, 4266367
    ),
    condition_start_date = as.Date(c(
      "2012-10-01", "2012-01-01", "2014-01-01", "2010-01-01", "2015-02-01",
      "2012-01-01", "2013-10-01", "2014-10-10"
    )),
    condition_end_date = as.Date(c(
      "2013-01-01", "2012-04-01", "2014-10-12", "2015-01-01", "2015-03-01",
      "2012-04-01", "2013-12-01", NA
    )),
    condition_type_concept_id = 32020
  )
  cdm <- mockPatientProfiles(
    connectionDetails,
    person = person, observation_period = observation_period,
    cohort_interest = cohort_interest, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence
  )

  concept <- dplyr::tibble(
    concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
    domain_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_class_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01")
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))

  cdm <- CDMConnector::insertTable(cdm, "concept", concept)


  test_data <- cdm$cohort_interest %>%
    addDemographics(
      ageGroup = list(c(0, 24), c(25, 150))
    ) %>%
    summariseLargeScaleCharacteristics(
      strata = list("age_group", c("age_group", "sex")),
      episodeInWindow = c("condition_occurrence", "drug_exposure"),
      minimumFrequency = 0
    )


  # levels_ordered <- c("-inf to -366", "-365 to -31", "-30 to -1", "0 to 0", "1 to 30", "31 to 365", "366 to inf")
  #
  # plot <- plotLargeScaleCharacteristics(
  #   data =  test_data %>% dplyr::filter(group_level  ==  "cohort_1"),
  #   xAxis = "variable_name",
  #   yAxis = "estimate_value",
  #   facetVarX = c("variable_level"),
  #   colorVars = c("group_level", "strata_level", "strata_name"),
  #   vertical_x = TRUE,
  #   facetOrder = levels_ordered
  # )
  # expect_true(ggplot2::is.ggplot(plot))#

  # levels_ordered <- c("-inf to -366.cohort_1", "-inf to -366.cohort_2",
  #                     "-365 to -31.cohort_1", "-365 to -31.cohort_2")
  plot_multiple <- plotLargeScaleCharacteristics(
    data =  test_data %>% dplyr::filter(group_level  %in% c("cohort_1", "cohort_2")),
    xAxis = "variable_name",
    yAxis = "estimate_value",
    facetVarX = c("variable_level",  "group_level"),
    colorVars = c("strata_level", "strata_name")
  )

  expect_true(ggplot2::is.ggplot(plot_multiple))

  #do not throw error even if they do not specify color or facet
  expect_no_error(plotLargeScaleCharacteristics(
    data =  test_data,
    xAxis = "variable_name",
    yAxis = "estimate_value"))

})
