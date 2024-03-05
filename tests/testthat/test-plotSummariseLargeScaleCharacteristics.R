# Test 1: Function returns a ggplot object
test_that("Function returns a ggplot object", {
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
  cdm <- mockPatientProfiles(
    drug_exposure = drug_exposure,
    connectionDetails
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
  name <- CDMConnector::inSchema(
    schema = connectionDetails$write_schema, table = "concept"
  )
  DBI::dbWriteTable(
    conn = connectionDetails$con, name = name, value = concept, overwrite = TRUE
  )
  cdm$concept <- dplyr::tbl(connectionDetails$con, name)

  test_data <- summariseLargeScaleCharacteristics(cdm$cohort1, eventInWindow = "drug_exposure")
  plot <- plotSummarisedLargeScaleCharacteristics(
    data =  test_data,
    cohort_name = "cohort_1",
    compare_cohorts = FALSE,
    facet_vars = c("estimate_type"),
    color_vars = c("variable_level", "group_level")
  )
  expect_true(ggplot2::is.ggplot(plot))
})
