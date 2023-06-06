test_that("addAttributes, functionality", {
  cdm <- mockPatientProfiles(connectionDetails)

  oldCohort <- cdm$cohort1
  newCohort <- cdm$cohort1 %>%
    addDemographics(cdm)

  newCohort <- newCohort %>%
    addAttributes(oldCohort)

  expect_true(length(attributes(newCohort)) == length(attributes(oldCohort)))
  for (i in names(attributes(newCohort))) {
    if (i != "names" && i != "class") {
      expect_true(identical(attr(newCohort, i), attr(oldCohort, i)))
    }
  }
})

test_that("addAttributes, expected errors", {
  cdm <- mockPatientProfiles(connectionDetails)

  expect_error(addAttributes(cdm))
  expect_error(addAttributes(cdm$cohort1, "cdm$cohort2"))
})
