test_that("test summariseCharacteristics", {
  cdm <- PatientProfiles::mockPatientProfiles()
  expect_no_error(res <- PatientProfiles::summariseDemographics(
    cohort = cdm$cohort1
  ))

  expect_no_error(res <- PatientProfiles::summariseDemographics(
    cohort = cdm$cohort1,
    ageGroup = list(c(0,50), c(51,100))
  ))

  cdm$cohort1 <- cdm$cohort1 %>%
    PatientProfiles::addSex() %>%
    dplyr::compute()

  expect_no_error(
    res <- PatientProfiles::summariseDemographics(
    cohort = cdm$cohort1,
    strata = list(c("sex")),
    ageGroup = list(c(0,50), c(51,100))
  ))
  expect_equal(
    res <- PatientProfiles::summariseDemographics(
      cohort = cdm$cohort1,
      strata = list(c("sex")),
      ageGroup = list(c(0,50), c(51,100))
    ) |>
      dplyr::filter(variable_level == "Female", strata_level == "overall", estimate_name == "percentage") |>
      dplyr::pull("estimate_value") %>%
      as.numeric(),
    100
  )

  expect_equal(
    res <- PatientProfiles::summariseDemographics(
      cohort = cdm$cohort1,
      strata = list(c("sex")),
      ageGroup = list(c(0,50), c(51,100))
    ) |>
      dplyr::select("additional_level")%>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull("n"),
    1
  )

  expect_equal(
    res <- PatientProfiles::summariseDemographics(
      cohort = cdm$cohort1,
      strata = list(c("sex")),
      ageGroup = list(c(0,50), c(51,100))
    ) |>
      dplyr::select("additional_level")%>%
      dplyr::distinct() %>%
      dplyr::pull("additional_level"),
    "overall"
  )

  expect_equal(
    res <- PatientProfiles::summariseDemographics(
      cohort = cdm$cohort1,
      strata = list(c("sex")),
      ageGroup = list(c(0,50), c(51,100))
    ) |>
      dplyr::select("additional_name")%>%
      dplyr::distinct() %>%
      dplyr::tally() %>%
      dplyr::pull("n"),
    1
  )

  expect_equal(
    res <- PatientProfiles::summariseDemographics(
      cohort = cdm$cohort1,
      strata = list(c("sex")),
      ageGroup = list(c(0,50), c(51,100))
    ) |>
      dplyr::select("additional_name")%>%
      dplyr::distinct() %>%
      dplyr::pull("additional_name"),
    "overall"
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})
