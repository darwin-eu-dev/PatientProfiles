test_that("addAttributes, functionality", {
  cdm <- mockPatientProfiles(connectionDetails)

  oldCohort <- cdm$cohort1
  newCohort <- cdm$cohort1 %>%
    addDemographics(cdm)

  expect_true(length(attributes(newCohort)) == length(attributes(oldCohort)))
  for (i in names(attributes(newCohort))) {
    if (i != "tbl_name") {
      x <- attr(newCohort, i)
      y <- attr(oldCohort, i)
      if (i == "class") {
        x <- x[x != "GeneratedCohortSet"]
        y <- y[y != "GeneratedCohortSet"]
      }
      expect_true(identical(x ,y))
    }
  }

  oldCohort <- cdm$cohort1
  newCohort <- cdm$cohort1 %>%
    addCohortIntersect(targetCohortTable = "cohort2")

  expect_true(length(attributes(newCohort)) == length(attributes(oldCohort)))
  for (i in names(attributes(newCohort))) {
    if (i != "tbl_name") {
      x <- attr(newCohort, i)
      y <- attr(oldCohort, i)
      if (i == "class") {
        x <- x[x != "GeneratedCohortSet"]
        y <- y[y != "GeneratedCohortSet"]
      }
      expect_true(identical(x ,y))
    }
  }
})
