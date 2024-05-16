test_that("errors for mock", {
  expect_error(mockPatientProfiles(con = "aasa"))
  expect_no_error(cdm <- mockPatientProfiles())
  expect_true(inherits(cdm, "cdm_reference"))
  mockDisconnect(cdm = cdm)
})
