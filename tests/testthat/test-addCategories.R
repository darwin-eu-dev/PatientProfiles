test_that("addCategories, functionality", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)
  agegroup <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(0, 40), c(41, 120)))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date)

  agegroupOverlap <- cdm$cohort1 %>%
    addAge(cdm) %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(0, 55), c(50, 120))),
      overlap = TRUE
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(agegroup %>%
    dplyr::select(age_group) %>%
    dplyr::pull() ==
    c("41 to 120", "41 to 120", "41 to 120", "41 to 120")))

  expect_true(all(agegroupOverlap %>%
    dplyr::select(age_group) %>%
    dplyr::pull() ==
    c(
      "0 to 55 and 50 to 120", "0 to 55 and 50 to 120", "50 to 120", "50 to 120"
    )))
})

test_that("addCategories with infinity", {
  table <- dplyr::tibble(
    subject_id = 1:6,
    prior_history = c(1, 8, Inf, -Inf, 20, NA),
    date = as.Date(c(
      "2020-01-01", NA, "2020-12-21", "2020-08-01", "2025-01-01", "2020-01-18"
    ))
  )
  name <- CDMConnector::inSchema(connectionDetails$write_schema, "test_table")
  DBI::dbWriteTable(connectionDetails$con, name = name, value = table)
  table <- dplyr::tbl(connectionDetails$con, name) %>%
    addCategories(
      variable = "prior_history", categories = list(
        "prior_group" = list(c(1, 10), c(11, Inf))
      ), missingCategoryValue = "None", overlap = FALSE
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$subject_id)
  expect_true("prior_group" %in% colnames(table))
  expect_true(all(table$prior_group == c(" 1 to 10", "1 to 10", "11 to Inf", "None", "11 to Inf")))
  DBI::dbRemoveTable(connectionDetails$con, name = name)
})
