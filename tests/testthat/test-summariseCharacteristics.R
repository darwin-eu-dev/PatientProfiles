test_that("test all functions", {
  x <- dplyr::tibble(
    s  = c("g1", "g1", "g2", "g1&&g2", "g2", "g1&&g2"),
    v1 = c(1, 2, 3, 4, 6, 3),
    v2 = c("a", "b", "a&&b", "b", "0", "0&&ab"),
    v3 = c(0, 1, 0, 1, 1, 0),
    v4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )
  s1 <- summariseCharacteristics(x)
  s2 <- summariseCharacteristics(x, strata = list("group" = "s"))
  s3 <- summariseCharacteristics(
    x, strata = list("group" = "s"), suppressCellCount = 1
  )
  s4 <- summariseCharacteristics(
    x, strata = list("group1" = c("s", "v2"), group2 = "s"),
    suppressCellCount = 1
  )

  x <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 1, 2),
    cohort_start_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    cohort_end_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    acetaminophen_m365_to_0 = c(1, 1, 0),
    ibuprophen_m365_to_0 = c(0, 0, 0),
    naloxone_m365_to_0 = c(0, 0, 0),
    headache_minf_to_0 = c(0, 1, 0),
    covid_minf_to_0 = c(1, 1, 0)
  )
  expect_no_error(summariseCharacteristics(
    x, strata = list(), suppressCellCount = 1
  ))
})
