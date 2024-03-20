test_that("summarise cohortIntersect", {

  cdm <- PatientProfiles::mockPatientProfiles()
  expect_error(res <- PatientProfiles::summariseCohortIntersect(
    cohort = cdm$cohort1
  ))

  expect_no_error(PatientProfiles::summariseCohortIntersect(
    cohort = cdm$cohort1, targetCohort = list(targetCohortTable = "cohort2",value = "flag", window = c(0,0))
  ))


  expect_no_error(PatientProfiles::summariseCohortIntersect(
    cohort = cdm$cohort1,
    targetCohort = list(
      targetCohortTable = "cohort2",
      value = "flag",
      window = c(0, 0)
    ),
    ageGroup = list(c(0, 19), c(20, 150))
  ))

})

