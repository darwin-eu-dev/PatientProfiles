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

  expect_true(inherits(res, "summarised_result"))

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

test_that("plotDemographics", {
  cdm <- mockPatientProfiles()

  results <- summariseDemographics(cdm$cohort2)

  gg1 <- plotDemographics(results,
                          xAxis = "estimate_value",
                          yAxis = "variable_name",
                          plotStyle = "boxplot",
                          vertical_x = TRUE)
  expect_true(ggplot2::is.ggplot(gg1))

  gg2 <- plotDemographics(
    data =  results,
    xAxis = "estimate_value",
    yAxis = "variable_name",
    plotStyle = "barplot",
    facetVarX = c("group_level"),
    colorVars = c("variable_name", "variable_level")
  )

  expect_true(ggplot2::is.ggplot(gg2))

  gg3 <- plotCharacteristics(
    data =  results,
    xAxis = "estimate_value",
    yAxis = "variable_name",
    plotStyle = "boxplot",
    facetVarX = "variable_name",
    colorVars = c("group_level")
  )

  expect_true(ggplot2::is.ggplot(gg3))

  CDMConnector::cdm_disconnect(cdm)
})
