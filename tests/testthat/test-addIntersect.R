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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", nameStyle = "xx") %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(length(attributes(cdm$cohort1)) == length(attributes(result)))
  for (i in names(attributes(cdm$cohort1))) {
    if (i != "names" && i != "class") {
      expect_true(identical(attr(cdm$cohort1, i), attr(result, i)))
    }
  }

  expect_true(colnames(result)[1] == "cohort_definition_id")
  expect_true(colnames(result)[2] == "subject_id")
  expect_true(colnames(result)[3] == "cohort_start_date")
  expect_true(colnames(result)[4] == "cohort_end_date")

  expect_true(all(result$xx == as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  result1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "count") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "days") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "flag") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result1$count_all_0_to_inf == c(4, 4, 3, 3, 1)))
  expect_true(all(result1$days_all_0_to_inf == c(14, 0, 5, 23, 43)))
  expect_true(all(result1$flag_all_0_to_inf == c(1, 1, 1, 1, 1)))

  result2 <-
    cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "count", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "flag", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "days", order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result6 <-
    cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = c("date", "count", "days", "flag"), order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  for (col in colnames(result2)) {
    expect_true(all(result2[[col]][!is.na(result2[[col]])] == result6[[col]][!is.na(result6[[col]])]))
  }

  expect_true(all(result2$date_all_0_to_inf == as.Date(
    c(
      "2020-02-16",
      "2020-02-16",
      "2020-02-16",
      "2020-03-15",
      "2020-03-15"
    )
  )))
  expect_true(all(result2$days_all_0_to_inf == c(46, 32, 27, 74, 43)))
  expect_true(all(result2$count_all_0_to_inf == c(4, 4, 3, 3, 1)))
  expect_true(all(result2$flag_all_0_to_inf == c(1, 1, 1, 1, 1)))

  result3 <-
    cdm$cohort1 %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "date"
    ) %>%
    addIntersect(
      cdm = cdm,
      tableName = "cohort2",
      window = list(c(-Inf, 0)), value = "days"
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

  expect_true(all(result3$date_all_minf_to_0 %in% as.Date(
    c(NA, "2020-01-15", "2020-01-15", NA, "2020-01-24")
  )))
  expect_true(all(result3$days_all_minf_to_0 %in% c(NA, 0, -5, NA, -8)))
  expect_true(all(result3$count_all_minf_to_0 == c(0, 1, 1, 0, 2)))
  expect_true(all(result3$flag_all_minf_to_0 == c(0, 1, 1, 0, 1)))

  result4 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "date") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "days") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "count") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "flag") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result4$date_all_m30_to_30 == as.Date(
    c(
      "2020-01-15",
      "2020-01-15",
      "2020-01-15",
      "2020-01-24",
      "2020-01-24"
    )
  )))
  expect_true(all(result4$days_all_m30_to_30 == c(14, 0, -5, 23, -8)))
  expect_true(all(result4$count_all_m30_to_30 == c(3, 3, 4, 2, 2)))
  expect_true(all(result4$flag_all_m30_to_30 == c(1, 1, 1, 1, 1)))

  result5 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "date", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "days", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "count", order = "last") %>%
    addIntersect(cdm = cdm, tableName = "cohort2", window = list(c(-30, 30)), value = "flag", order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()


  expect_true(all(result5$count_all_m30_to_30 == c(3, 3, 4, 2, 2)))
  expect_true(all(result5$flag_all_m30_to_30 == c(1, 1, 1, 1, 1)))
  expect_true(all(result5$days_all_m30_to_30 == c(25, 11, 27, 28, -3)))
  expect_true(all(result5$date_all_m30_to_30 == as.Date(
    c(
      "2020-01-26",
      "2020-01-26",
      "2020-02-16",
      "2020-01-29",
      "2020-01-29"
    )
  )))
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", value = "date",
      indexDate = "cohort_end_date"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$date_all_0_to_inf == as.Date(c("2020-01-25", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))
})

test_that("working examples with extra column", {
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  cdm$cohort2 <- cdm$cohort2 %>%
    dbplyr::window_order(
      .data$cohort_definition_id, .data$subject_id, .data$cohort_start_date
    ) %>%
    dplyr::mutate(measurment_result = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    CDMConnector::computeQuery()

  result <- cdm$cohort1 %>%
    addIntersect(cdm, "cohort2", c("flag", "measurment_result"), "cohort_definition_id", 1, "covid", list(c(0, Inf))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result1 <- cdm$cohort1 %>%
    addIntersect(cdm, "cohort2", "measurment_result", "cohort_definition_id", 2, "covid", list(c(0, Inf))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-15",
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
        "2020-01-15",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )



  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  cdm$cohort2 <- cdm$cohort2 %>%
    dbplyr::window_order(
      .data$cohort_definition_id, .data$subject_id, .data$cohort_start_date
    ) %>%
    dplyr::mutate(measurment_result = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    CDMConnector::computeQuery()

  result2 <- cdm$cohort1 %>%
    addIntersect(cdm, "cohort2", "measurment_result",
      nameStyle = "{value}_{window_name}"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result3 <- cdm$cohort1 %>%
    addIntersect(cdm, "cohort2", c("flag", "measurment_result"),
      nameStyle = "{value}_{window_name}",
      window = list(c(-400, -200))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$measurment_result_covid_0_to_inf == c(1, 1, 2, 5, 7)))
  expect_true(all(is.na(result1$measurment_result_covid_0_to_inf)))
  expect_true(all(result2$measurment_result_0_to_inf == c("1; 2", "1; 2", 3, 5, 7)))
  expect_true(all(is.na(result3$measurment_result_m400_to_m200)))
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

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

  expect_true(all(compareNA(result$date_id1_0_to_inf, as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", NA, NA)))))

  result1 <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = 2, value = "count"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result1$count_id2_0_to_inf == c(1, 1, 1, 1, 0)))

  result2 <- cdm$cohort1 %>%
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
      filterId = c(1, 3), value = "days"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result3 <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", filterVariable = "cohort_definition_id",
      filterId = c(1, 3), value = c("count", "days", "flag")
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  for (col in colnames(result2)) {
    expect_true(all(result2[[col]][!is.na(result2[[col]])] == result3[[col]][!is.na(result3[[col]])]))
  }

  expect_true(all(result2$count_id1_0_to_inf == c(2, 2, 1, 0, 0)))
  expect_true(all(compareNA(result2$days_id1_0_to_inf, c(14, 0, 5, NA, NA))))
  expect_true(all(result2$flag_id1_0_to_inf == c(1, 1, 1, 0, 0)))
  expect_true(all(result2$count_id3_0_to_inf == c(1, 1, 1, 2, 1)))
  expect_true(all(result2$days_id3_0_to_inf == c(46, 32, 27, 23, 43)))
  expect_true(all(result2$flag_id3_0_to_inf == c(1, 1, 1, 1, 1)))
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", nameStyle = "test_{id_name}_{window_name}") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", targetEndDate = NULL) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$test_all_0_to_inf == as.Date("2020-01-01")))
  # expect_true(("all_0_to_inf" %in% colnames(result1)))
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

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

  result1 <- cdm$cohort1 %>%
    addIntersect(cdm = cdm, tableName = "cohort2", value = "date", window = list(c(0, Inf), c(-Inf, 0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$date_all_0_to_inf == result1$date_all_0_to_inf))
  expect_true(all(compareNA(result$date_all_minf_to_0, result1$date_all_minf_to_0)))
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

  drugExposure <- dplyr::tibble(
    subject_id = c(1, 1),
    drug_concept_id = c(1, 2),
    drug_exposure_start_date = as.Date(c("2020-02-10", "2019-09-01"))
  )

  conditionOccurrence <- dplyr::tibble(
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

  cdm <- mockPatientProfiles(connectionDetails,
                             cohort1 = cohort1,
                             condition_occurrence = conditionOccurrence,
                             drug_exposure = drugExposure)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "condition_occurrence", value = "date",
      targetStartDate = "condition_occurrence_start_date",
      targetEndDate = "condition_occurrence_end_date",
      window = list(c(0, Inf), c(-Inf, 0))
    ) %>%
    dplyr::collect()

  expect_true(all(result$date_all_0_to_inf %in% as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  result1 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "count",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf), c(-Inf, 0)), filterVariable = "drug_concept_id",
      filterId = c(1, 2)
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()

  expect_true(all(result1$count_id1_0_to_inf == c(1, 1, 1, 0, 0, 0, 0)))
  # test output all zero column when no result found
  expect_true(all(result1$count_id2_0_to_inf == c(0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(result1$count_id1_minf_to_0 == c(0, 0, 0, 1, 0, 0, 0)))
  expect_true(all(result1$count_id2_minf_to_0 == c(1, 1, 1, 1, 0, 0, 0)))

  result2 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "count",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf), c(-Inf, 0))
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()

  expect_true(all(result1$count_id1_0_to_inf + result1$count_id2_0_to_inf == result2$count_all_0_to_inf))
  expect_true(all(result1$count_id1_minf_to_0 + result1$count_id2_minf_to_0 == result2$count_all_minf_to_0))

  result3 <- cdm$condition_occurrence %>%
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
  expect_true(all(is.na(result3$date_id2_0_to_inf)))

  result4 <- cdm$condition_occurrence %>%
    addIntersect(
      cdm = cdm, tableName = "drug_exposure", value = "days",
      indexDate = "condition_occurrence_start_date",
      targetStartDate = "drug_exposure_start_date", targetEndDate = NULL,
      window = list(c(0, Inf)), filterVariable = "drug_concept_id",
      filterId = c(1, 2)
    ) %>%
    dplyr::arrange(subject_id, condition_occurrence_start_date) %>%
    dplyr::collect()
  # test output all zero column when no result found
  expect_true(all(is.na(result4$days_id2_0_to_inf)))
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  expect_error(addIntersect("cdm$cohort1", cdm))

  expect_error(addIntersect(cdm$cohort1, "cdm"))

  expect_error(addIntersect(cdm$cohort1, "cdm", tableName = "drug"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "end_date"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", cohortId = "3"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", indexDate = 3))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", targetStartDate = "no"))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", targetEndDate = NA))

  expect_error(addIntersect(cdm$cohort1, cdm, tableName = "cohort2", value = "flag", nameStyle = "test_{nowindow}_{cohortName}"))
})

test_that("test checkWindow function", {
  cdm <- mockPatientProfiles(connectionDetails)

  expect_error(cdm$cohort1 %>%
    addIntersect(
      cdm = cdm,
      value = "days",
      filterId = 1,
      filterVariable = "cohort_definition_id",
      tableName = "cohort2",
      window = c(150, -90)
    ))
})

test_that("test if column exist, overwrite", {
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
    ),
    flag_all_0_to_30 = c(2, 2, 2, 2, 2),
    count_all_0_to_30 = c(1, 1, 1, 1, 1),
    days_all_0_to_30 = c(1, 1, 1, 1, 1),
    date_all_0_to_30 = c(1, 1, 1, 1, 1)
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2",
      value = c("flag", "date", "days", "count"), window = list(c(0, 30))
    ) %>%
    dplyr::collect()

  expect_true(sum(colnames(result) == "flag_all_0_to_30") == 1)
  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(flag_all_0_to_30) !=
    cohort1 %>%
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(flag_all_0_to_30), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(count_all_0_to_30) !=
    cohort1 %>%
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(count_all_0_to_30), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(days_all_0_to_30) !=
    cohort1 %>%
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(days_all_0_to_30), na.rm = TRUE))

  expect_true(all(result %>% dplyr::arrange(cohort_start_date, subject_id) %>%
    dplyr::select(date_all_0_to_30) !=
    cohort1 %>%
      dplyr::arrange(cohort_start_date, subject_id) %>%
      dplyr::select(date_all_0_to_30), na.rm = TRUE))
})

test_that("overlapTable is empty, check return columns", {
  # functionality
  cohort2 <- dplyr::tibble(
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

  cohort1 <- dplyr::tibble(
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)


  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2", value = c("date", "days", "count", "flag"),
      filterVariable = "cohort_definition_id",
      filterId = 2
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(c(
    "count_id2_0_to_inf", "flag_id2_0_to_inf", "days_id2_0_to_inf",
    "date_id2_0_to_inf"
  ) %in% colnames(result)))

  expect_true(all(result$count_id2_0_to_inf == 0))

  expect_true(all(result$flag_id2_0_to_inf == 0))

  expect_true(all(is.na(result$days_na_0_to_inf)))

  expect_true(all(is.na(result$date_id2_0_to_inf)))
})

test_that("overlap is empty or not, multiple ids, check return columns", {
  # functionality
  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 3),
    subject_id = c(1, 1, 1, 2, 2, 3),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01",
        "2020-03-03"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01",
        "2020-03-03"
      )
    )
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(3, 3, 3, 3, 3, 3, 3),
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

  cdm <- mockPatientProfiles(connectionDetails, cohort1 = cohort1, cohort2 = cohort2)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addCohortIntersectCount(
      cdm = cdm, targetCohortTable = "cohort2"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true("cohort_1_0_to_inf" %in% colnames(result))

  expect_true(all(result$cohort_1_0_to_inf == 0))

  result <- cdm$cohort1 %>%
    addCohortIntersectFlag(
      cdm = cdm, targetCohortTable = "cohort2"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true("cohort_1_0_to_inf" %in% colnames(result))

  expect_true(all(result$cohort_1_0_to_inf == 0))

  result <- cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm, targetCohortTable = "cohort2"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true("cohort_1_0_to_inf" %in% colnames(result))

  expect_true(all(is.na(result$cohort_1_0_to_inf)))

  result <- cdm$cohort1 %>%
    addCohortIntersectDays(
      cdm = cdm, targetCohortTable = "cohort2"
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true("cohort_1_0_to_inf" %in% colnames(result))

  expect_true(all(is.na(result$cohort_1_0_to_inf)))

  result <- cdm$cohort1 %>%
    addIntersect(
      cdm = cdm, tableName = "cohort2",
      value = c("flag", "date"),
      filterVariable = "cohort_definition_id",
      filterId = c(1, 2, 3),
      window = list(c(0, Inf), c(-30, -1)),
      idName = c("num1", "num2", "num3")
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(c(
    "flag_num1_0_to_inf", "flag_num2_0_to_inf", "flag_num3_0_to_inf",
    "date_num1_0_to_inf", "date_num2_0_to_inf", "date_num3_0_to_inf",
    "flag_num1_m30_to_m1", "flag_num2_m30_to_m1", "flag_num3_m30_to_m1",
    "date_num1_m30_to_m1", "date_num2_m30_to_m1", "date_num3_m30_to_m1"
  )
  %in% colnames(result)))

  expect_true(all(compareNA(result$date_num3_0_to_inf, c("2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", NA))))
  expect_true(all(compareNA(result$date_num3_m30_to_m1, c(NA, NA, NA, NA, NA, NA, "2020-03-03"))))
  expect_true(all(result$flag_num3_0_to_inf == c(1, 1, 1, 1, 1, 1, 0)))
  expect_true(all(result$flag_num3_m30_to_m1 == c(0, 0, 0, 0, 0, 0, 1)))

  expect_true(all(result$flag_num2_0_to_inf == c(0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(result$flag_num1_0_to_inf == c(0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(result$flag_num2_m30_to_m1 == c(0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(result$flag_num1_m30_to_m1 == c(0, 0, 0, 0, 0, 0, 0)))

  expect_true(all(is.na(result$date_num2_0_to_inf)))
  expect_true(all(is.na(result$date_num1_0_to_inf)))
  expect_true(all(is.na(result$date_num2_m30_to_m1)))
  expect_true(all(is.na(result$date_num1_m30_to_m1)))

  expect_error(cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm,
      targetCohortTable = "cohort2",
      targetCohortId = c(1, 2, 3),
      window = list(c(0, Inf), c(-30, -1))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect())

  result <- cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm,
      targetCohortTable = "cohort2",
      targetCohortId = c(1, 3),
      window = list(c(0, Inf), c(-30, -1))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(c(
    "cohort_3_m30_to_m1", "cohort_1_m30_to_m1",
    "cohort_3_0_to_inf", "cohort_1_0_to_inf"
  )
  %in% colnames(result)))

  expect_true(all(compareNA(result$cohort_3_0_to_inf, c("2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", "2020-03-03", NA))))
  expect_true(all(compareNA(result$cohort_3_m30_to_m1, c(NA, NA, NA, NA, NA, NA, "2020-03-03"))))

  expect_true(all(is.na(result$cohort_1_m30_to_m1)))
  expect_true(all(is.na(result$cohort_1_0_to_inf)))
})
