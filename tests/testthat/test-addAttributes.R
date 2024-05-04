test_that("attributes and classes are kept", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  oldCohort <- cdm$cohort1
  newCohort <- cdm$cohort1 %>% addDemographics()

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
    addCohortIntersectFlag(targetCohortTable = "cohort2") |>
    addTableIntersectCount(tableName = "condition_occurrence")

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
