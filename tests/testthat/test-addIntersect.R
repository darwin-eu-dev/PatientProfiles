test_that("working examples", {
  # functionality
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
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", nameStyle = "xx") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$xx == as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  result_1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "count") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "time") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "flag") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$count_NA_0_to_Inf == c(4, 4, 3, 3, 1)))
  expect_true(all(result_1$time_NA_0_to_Inf == c(14, 0, 5, 23, 43)))
  expect_true(all(result_1$flag_NA_0_to_Inf == c(1, 1, 1, 1, 1)))

  result_2 <-
    cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "count", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "flag", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "time", order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_6 <-
    cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = c("date", "count", "time", "flag"), order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  for (col in colnames(result_2)) {
    expect_true(all(result_2[[col]][!is.na(result_2[[col]])] == result_6[[col]][!is.na(result_6[[col]])]))
  }

  expect_true(all(result_2$date_NA_0_to_Inf == as.Date(
    c(
      "2020-02-16",
      "2020-02-16",
      "2020-02-16",
      "2020-03-15",
      "2020-03-15"
    )
  )))
  expect_true(all(result_2$time_NA_0_to_Inf == c(46, 32, 27, 74, 43)))
  expect_true(all(result_2$count_NA_0_to_Inf == c(4, 4, 3, 3, 1)))
  expect_true(all(result_2$flag_NA_0_to_Inf == c(1, 1, 1, 1, 1)))

  result_3 <-
    cdm$cohort1 %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "date"
    ) %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "time"
    ) %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "count"
    ) %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "flag"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_3$date_NA_mInf_to_0 %in% as.Date(
    c(NA, "2020-01-15", "2020-01-15", NA, "2020-01-24")
  )))
  expect_true(all(result_3$time_NA_mInf_to_0 %in% c(NA, 0, -5, NA, -8)))
  expect_true(all(result_3$count_NA_mInf_to_0 == c(0, 1, 1, 0, 2)))
  expect_true(all(result_3$flag_NA_mInf_to_0 == c(0, 1, 1, 0, 1)))

  result_4 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "date") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "time") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "count") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "flag") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_4$date_NA_m30_to_30 == as.Date(
    c(
      "2020-01-15",
      "2020-01-15",
      "2020-01-15",
      "2020-01-24",
      "2020-01-24"
    )
  )))
  expect_true(all(result_4$time_NA_m30_to_30 == c(14, 0, -5, 23, -8)))
  expect_true(all(result_4$count_NA_m30_to_30 == c(3, 3, 4, 2, 2)))
  expect_true(all(result_4$flag_NA_m30_to_30 == c(1, 1, 1, 1, 1)))

  result_5 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "date", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "time", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "count", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "flag", order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_5$count_NA_m30_to_30 == c(3, 3, 4, 2, 2)))
  expect_true(all(result_5$flag_NA_m30_to_30 == c(1, 1, 1, 1, 1)))
  expect_true(all(result_5$time_NA_m30_to_30 == c(25, 11, 27, 28, -3)))
  expect_true(all(result_5$date_NA_m30_to_30 == as.Date(
    c(
      "2020-01-26",
      "2020-01-26",
      "2020-02-16",
      "2020-01-29",
      "2020-01-29"
    )
  )))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples with cohort_end_date", {
  # functionality
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
        "2020-01-20",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", value = "date",
      indexDate = "cohort_end_date"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$date_NA_0_to_Inf == as.Date(c("2020-01-25", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples with multiple cohort Ids", {
  # functionality
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = 1, value = "date"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(compareNA(result$date_id1_0_to_Inf, as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", NA, NA)))))

  result_1 <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = 2, value = "count"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$count_id2_0_to_Inf == c(1, 1, 1, 1, 0)))

  result_2 <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = c(1, 3), value = "flag"
    ) %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = c(1, 3), value = "count"
    ) %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = c(1, 3), value = "time"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_3 <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = c(1, 3), value = c("count", "time", "flag")
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  for (col in colnames(result_2)) {
    expect_true(all(result_2[[col]][!is.na(result_2[[col]])] == result_3[[col]][!is.na(result_3[[col]])]))
  }

  expect_true(all(result_2$count_id1_0_to_Inf == c(2, 2, 1, 0, 0)))
  expect_true(all(compareNA(result_2$time_id1_0_to_Inf, c(14, 0, 5, NA, NA))))
  expect_true(all(result_2$flag_id1_0_to_Inf == c(1, 1, 1, 0, 0)))
  expect_true(all(result_2$count_id3_0_to_Inf == c(1, 1, 1, 2, 1)))
  expect_true(all(result_2$time_id3_0_to_Inf == c(46, 32, 27, 23, 43)))
  expect_true(all(result_2$flag_id3_0_to_Inf == c(1, 1, 1, 1, 1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples calculating as incidence target cohort", {
  # functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2021-01-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-02-15"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", nameStyle = "test_{id_name}_{window_name}") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", targetEndDate = NULL) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$test_NA_0_to_Inf == as.Date("2020-01-01")))
  # expect_true(("all_0_to_Inf" %in% colnames(result_1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples with more than one window", {
  # functionality
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", window = list(c(-Inf, 0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", window = list(c(0, Inf), c(-Inf, 0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$date_NA_0_to_Inf == result_1$date_NA_0_to_Inf))
  expect_true(all(compareNA(result$date_NA_mInf_to_0, result_1$date_NA_mInf_to_0)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples with tables, not cohorts", {
  # functionality
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

  drug_exposure <- dplyr::tibble(
    subject_id = c(1, 1),
    drug_concept_id = c(1, 2),
    drug_exposure_start_date = as.Date(c("2020-02-10", "2019-09-01"))
  )

  condition_occurrence <- dplyr::tibble(
    condition_concept_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    condition_occurrence_start_date = as.Date(
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
    condition_occurrence_end_date = as.Date(
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, condition_occurrence = condition_occurrence, drug_exposure = drug_exposure)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "condition_occurrence", value = "date",
      targetStartDate = "condition_occurrence_start_date",
      targetEndDate = "condition_occurrence_end_date"
    ) %>%
    dplyr::collect()

  expect_true(all(result$date_NA_0_to_Inf %in% as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  result_1 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "count",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf), c(-Inf, 0)), filterVariable = "drug_concept_id",
      filterId = c(1, 2)
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$count_id1_0_to_Inf == c(1, 1, 1, 0, 0, 0, 0)))
  # test output all zero column when no result found
  expect_true(all(result_1$count_id2_0_to_Inf == c(0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(result_1$count_id1_mInf_to_0 == c(0, 0, 0, 1, 0, 0, 0)))
  expect_true(all(result_1$count_id2_mInf_to_0 == c(1, 1, 1, 1, 0, 0, 0)))

  result_2 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "count",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf), c(-Inf, 0))
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$count_id1_0_to_Inf + result_1$count_id2_0_to_Inf == result_2$count_NA_0_to_Inf))
  expect_true(all(result_1$count_id1_mInf_to_0 + result_1$count_id2_mInf_to_0 == result_2$count_NA_mInf_to_0))

  result_3 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "date",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf)), filterVariable = "drug_concept_id",
      filterId = c(1, 2)
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()
  # test output all zero column when no result found
  expect_true(all(is.na(result_3$date_id2_0_to_Inf)))

  result_4 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "time",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf)), filterVariable = "drug_concept_id",
      filterId = c(1, 2)
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()
  # test output all zero column when no result found
  expect_true(all(is.na(result_4$time_id2_0_to_Inf)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check input length and type for each of the arguments", {
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
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  expect_error(addIntersect("cdm$cohort1", cdm))

  expect_error(addIntersect(cdm$cohort1, "cdm"))

  expect_error(addIntersect(cdm$cohort1, "cdm", tableName = "drug"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "end_date"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", cohortId = "3"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", indexDate = 3))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", targetStartDate = "no"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", targetEndDate = NA))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", nameStyle = "test_{nowindow}_{cohortName}"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
