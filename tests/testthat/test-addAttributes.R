test_that("addAttributes, functionality", {
  cdm <- mockPatientProfiles()

  oldcohort <- cdm$cohort1
  newcohort <- cdm$cohort1 %>%
    addDemographics(cdm)

  newcohort <- newcohort %>%
    addAttributes(oldcohort)

  expect_true(length(attributes(newcohort)) == length(attributes(oldcohort)))
  for(i in names(attributes(newcohort))) {
    if(i != "names" && i != "class") {
      expect_true(identical(attr(newcohort, i), attr(oldcohort, i)))
    }
  }

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("addAttributes, expected errors", {
  cdm <- mockPatientProfiles()

  expect_error(addAttributes(cdm))
  expect_error(addAttributes(cdm$cohort1, "cdm$cohort2"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
