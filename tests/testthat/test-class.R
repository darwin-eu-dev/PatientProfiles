test_that("test class consistency across cohort operations", {
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2010-04-19", "2020-04-19", "2021-01-14", "2012-05-25"
    )),
    cohort_end_date = as.Date(c(
      "2010-09-19", "2020-08-10", "2021-11-14", "2022-09-25"
    ))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2006-03-11", "2006-03-11")),
    observation_period_end_date = as.Date(c("2102-04-02", "2102-04-02")),
    period_type_concept_id = c(0, 0)
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    dus_cohort = dus_cohort,
    observation_period = observation_period
  )

  operations <- list(
    "addInObservation" = addInObservation,
    "addSex" = addSex,
    "addAge" = addAge,
    "addCdmName" = addCdmName,
    "addCohortIntersectDays" = function(x) {
      addCohortIntersectDays(x, targetCohortTable = "cohort2")
    },
    "addCohortIntersectDate" = function(x) {
      addCohortIntersectDate(x, targetCohortTable = "cohort2")
    },
    "addCohortIntersectCount" = function(x) {
      addCohortIntersectCount(x, targetCohortTable = "cohort2")
    },
    "addCohortIntersectFlag" = function(x) {
      addCohortIntersectFlag(x, targetCohortTable = "cohort2")
    },
    "addCohortName" = addCohortName,
    "addDateOfBirth" = addDateOfBirth,
    "addDemographics" = addDemographics,
    "addFutureObservation" = addFutureObservation,
    "addTableIntersectCount" = function(x) {
      addTableIntersectCount(x, tableName = "drug_exposure")
    }
  )

  baseline_class <- class(cdm$cohort1)
  baseline_class <- baseline_class[baseline_class != "GeneratedCohortSet"]
  # Apply each operation to cdm$cohort1 and check the class consistency
  for (op_name in names(operations)) {
    op <- operations[[op_name]]
    result <- op(cdm$cohort1)
    class(result) <- class(result)[class(result) != "GeneratedCohortSet"]
    expect_identical(class(result), baseline_class,
      info = paste("Testing operation:", op_name)
    )
  }

  result_with_sequence <- cdm$cohort1 |>
    addSex() |>
    addAge() |>
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(0, 120))),
      overlap = TRUE
    )
  class(result_with_sequence) <- class(result_with_sequence)[class(result_with_sequence) != "GeneratedCohortSet"]

  expect_identical(class(result_with_sequence), baseline_class,
    info = "Testing sequence with addSex, addAge, and addCategories"
  )

  mockDisconnect(cdm = cdm)
})
