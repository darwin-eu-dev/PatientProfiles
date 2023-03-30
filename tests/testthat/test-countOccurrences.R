test_that("working examples", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  result <- cdm$cohort1 %>%
    countOccurrences(cdm = cdm,tableName = "cohort2") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$all_0_to_Inf == c(2,2,1,2,1)))

  result_1 <- cdm$cohort1 %>%
    countOccurrences(cdm = cdm, tableName = "cohort2", cohortId = c(2,3),
                     window = list(c(-Inf, 0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$cohort2_mInf_to_0 == c(0, 0, 0, 0, 1)))
  expect_true(all(result_1$cohort3_mInf_to_0 == c(0, 0, 0, 0, 1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
