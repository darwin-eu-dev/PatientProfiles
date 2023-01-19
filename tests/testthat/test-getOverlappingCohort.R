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

  cdm <- mockDrugUtilisation(cohort1 = cohort1, cohort2 = cohort2)

  result <- getOverlappingCohortSubjects(
    cdm = cdm,
    targetCohortName = "cohort1",
    targetCohortId = c("1"),
    overlapCohortName = "cohort2",
    overlapCohortId = c("2", "3"),
    lookbackWindow = c(-180, 0)
  )
  result <- result%>% dplyr::collect()
  expect_true(all(result[result$subject_id == 1,]$overlap_cohort2_2 == c(1,0)))
  expect_true(all(result[result$subject_id == 2,]$overlap_cohort2_2 == 1))
  expect_true(all(result[result$subject_id == 3,]$overlap_cohort2_2 == 0))

  expect_true(all(result[result$subject_id == 1,]$overlap_cohort2_3 == c(0,0)))
  expect_true(all(result[result$subject_id == 2,]$overlap_cohort2_3 == 1))
  expect_true(all(result[result$subject_id == 3,]$overlap_cohort2_3 == 0))
})

test_that("test multipleEvents",{
  cdm <- mockDrugUtilisation(
    cohort1 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 3),
      subject_id = c(1, 1, 2, 3, 4),
      cohort_start_date = as.Date(c(
        "2000-01-01", "2000-04-01", "2020-05-10", "2013-09-08", "1950-08-01"
      )),
      cohort_end_date = as.Date(c(
        "2000-03-01", "2001-04-01", "2021-05-10", "2014-09-08", "1951-08-01"
      ))
    ),
    cohort2 = tibble::tibble(
      cohort_definition_id = c(1, 1, 1, 2, 2, 2),
      subject_id = c(1, 1, 4, 1, 2, 4),
      cohort_start_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      )),
      cohort_end_date = as.Date(c(
        "1999-07-04", "2000-01-02", "1950-05-03", "1999-12-31", "2020-02-18",
        "1950-03-15"
      ))
    )
  )

  result <- getOverlappingCohortSubjects(
    cdm = cdm,
    targetCohortName = "cohort1",
    targetCohortId = 1,
    overlapCohortName = "cohort2",
    overlapCohortId = 1,
    lookbackWindow = c(NA, 0),
    multipleEvents = FALSE
  ) %>% dplyr::collect()
  expect_true(all(result$overlap_cohort2_1 == c(1,1,0)))

  result <- getOverlappingCohortSubjects(
    cdm = cdm,
    targetCohortName = "cohort1",
    targetCohortId = 1,
    overlapCohortName = "cohort2",
    overlapCohortId = 1,
    lookbackWindow = c(NA, 0),
    multipleEvents = TRUE
  ) %>% dplyr::collect()
  expect_true(all(result$overlap_cohort2_1 == c(1,2,0)))

  result <- getOverlappingCohortSubjects(
    cdm = cdm,
    targetCohortName = "cohort1",
    targetCohortId = 1,
    overlapCohortName = "cohort2",
    overlapCohortId = 1,
    lookbackWindow = NA,
    multipleEvents = TRUE
  ) %>% dplyr::collect()
  expect_true(all(result$overlap_cohort2_1 == c(2,2,0)))

})
