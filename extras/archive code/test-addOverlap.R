test_that("overlap function", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cohort2 <- tibble::tibble(
    cohort_definition_id = c(2, 2, 2, 3),
    subject_id = c(1, 2, 2, 2),
    cohort_start_date = c(
      as.Date("2009-12-01"), as.Date("2009-01-01"),
      as.Date("2009-11-01"), as.Date("2009-11-01")),
    cohort_end_date = c(
      as.Date("2011-01-01"), as.Date("2009-01-02"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cdm <- mockCohortProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result_1 <- addOverlap(
    cdm$cohort1,
    cdm = cdm,
    overlapCohortName = "cohort2",
    filter = NULL,
    window = c(0, 0),
    name = "overlap",
    compute = TRUE
  )

  result_2 <- addOverlap(
    cdm$cohort1,
    cdm = cdm,
    overlapCohortName = "cohort2",
    filter = NULL,
    window = c(0, -50),
    name = "over_lap",
    compute = TRUE
  )

  expect_true("overlap" %in% colnames(result_1))
  expect_true(all(result_1$overlap == c(1,1,0,0)))
  expect_true(dplyr::count(result_1) %>% dplyr::collect() ==dplyr::count(cohort1))

  expect_true("over_lap" %in% colnames(result_2))
  expect_true(all(result_2$overlap == c(1,0,0,0)))
  expect_true(dplyr::count(result_2) %>% dplyr::collect() ==dplyr::count(cohort1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("filter", {


  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = c(
      as.Date("2010-01-01"), as.Date("2013-01-01"),
      as.Date("2010-01-02"), as.Date("2010-01-01")),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2015-01-01"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cohort2 <- tibble::tibble(
    cohort_definition_id = c(2, 2, 2, 3),
    subject_id = c(1, 2, 2, 2),
    cohort_start_date = c(
      as.Date("2009-12-01"), as.Date("2009-01-01"),
      as.Date("2009-11-01"), as.Date("2009-11-01")),
    cohort_end_date = c(
      as.Date("2011-01-01"), as.Date("2009-01-02"),
      as.Date("2015-01-01"), as.Date("2015-01-01"))
  )

  cdm <- mockCohortProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result_1 <- addOverlap(
    cdm$cohort1,
    cdm = cdm,
    overlapCohortName = "cohort2",
    filter = list(cohort_definition_id = c(3),subject_id = c(2)),
    window = c(0, 0),
    name = "overlap",
    compute = TRUE
  )

  result_2 <- addOverlap(
    cdm$cohort1,
    cdm = cdm,
    overlapCohortName = "cohort2",
    filter = list(cohort_definition_id = c(2),subject_id = c(1,1)),
    window = c(0, 0),
    name = "overlap",
    compute = TRUE
  )

  expect_true("overlap" %in% colnames(result_1))
  expect_true(all(result_1$overlap == c(1,0,0,0)))
  expect_true(dplyr::count(result_1) %>% dplyr::collect() ==dplyr::count(cohort1))

  expect_true("overlap" %in% colnames(result_2))
  expect_true(all(result_2$overlap == c(1,0,0,0)))
  expect_true(all(result_2$cohort_start_date == c(1,0,0,0)))
  expect_true(dplyr::count(result_2) %>% dplyr::collect() ==dplyr::count(cohort1))



  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
