test_that("with and with overall groups and strata", {
  summaryChar <- dplyr::tibble(
    cdm_name = "PP_MOCK",
    result_type = "Summary characteristics",
    group_name = "Cohort name",
    group_level = c(rep("Cohort 1", 11), rep("Cohort 2", 11)),
    strata_name = "overall", strata_level = "overall",
    variable = rep(c(
      "Number subjects", "Number records", rep("Age", 5),
      rep("Medications prior year", 4)
    ), 2),
    variable_level = rep(
      c(rep(NA, 7), rep("Opioids", 2), rep("Naloxone", 2)), 2
    ),
    estimate_type = rep(c(
      "count", "count", "median", "min", "q25", "q75", "max", "count",
      "percentage", "count", "percentage"
    ), 2),
    estimate = as.character(c(
      100, 112, 42, 12, 25, 50, 89, 30, 26.78571, 10, 8.928571, 88, 88, 35, 12,
      20, 42, 83, 12, 13.63636, 5, 5.681818
    ))
  )
  expect_warning(
    x <- summaryChar %>% gtCharacteristics()
  )

  expect_false("percentage" %in% x$format)
})
