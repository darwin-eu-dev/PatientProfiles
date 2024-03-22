test_that("summarise cohortIntersect", {

  cdm <- PatientProfiles::mockPatientProfiles()
  expect_error(res <- PatientProfiles::summariseCohortIntersect(
    cohort = cdm$cohort1
  ))

  expect_no_error(PatientProfiles::summariseCohortIntersect(
    cohort = cdm$cohort1,
    cohortIntersect = list(
      targetCohortTable = "cohort2",value = "flag", window = c(0,0)
    )
  ))

})

