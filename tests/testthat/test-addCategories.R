test_that("addCategories, functionality", {
  skip_on_cran()
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = dplyr::tibble(
      "person_id" = 1:3, "gender_concept_id" = 0L, "year_of_birth" = 2000L,
      "race_concept_id" = 0L, "ethnicity_concept_id" = 0L
    ),
    cohort1 = dplyr::tibble(
      "cohort_definition_id" = 1L,
      "subject_id" = c(1L, 2L, 3L),
      "cohort_start_date" = as.Date(c("2045-01-01", "2052-01-01", "2060-01-01")),
      "cohort_end_date" = as.Date(c("2045-01-01", "2052-01-01", "2060-01-01"))
    )
  )
  agegroup <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(0, 49), c(50, 120)))
    ) %>%
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date)

  agegroupOverlap <- cdm$cohort1 %>%
    addAge() %>%
    addCategories(
      variable = "age",
      categories = list("age_group" = list(c(0, 55), c(50, 120))),
      overlap = TRUE
    ) %>%
    dplyr::collect() |>
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(
    agegroup %>%
      dplyr::pull(age_group) == c("0 to 49", "50 to 120", "50 to 120")
  ))

  expect_true(all(
    agegroupOverlap %>%
      dplyr::pull(age_group) == c("0 to 55", "0 to 55 and 50 to 120", "50 to 120")
  ))

  mockDisconnect(cdm)
})

test_that("addCategory with both upper and lower infinite, age", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_no_error(
    agegroup <- cdm$cohort1 %>%
      addAge() %>%
      addCategories(
        variable = "age",
        categories = list("age_group" = list(c(-Inf, Inf)))
      ) %>%
      dplyr::collect() |>
      dplyr::arrange(subject_id, cohort_start_date)
  )
  expect_true(
    all(agegroup %>%
      dplyr::pull("age_group") == "any")
  )

  expect_no_error(
    agegroup2 <- cdm$cohort1 %>%
      addAge() %>%
      addCategories(
        variable = "age",
        categories = list("age_group" = list(c(-Inf, 50), c(51, 120)))
      ) %>%
      dplyr::collect() |>
      dplyr::arrange(subject_id, cohort_start_date)
  )
  expect_true(
    "50 or below" %in% (
      agegroup2 %>% dplyr::pull("age_group")
    )
  )

  mockDisconnect(cdm)
})

test_that("addCategories with infinity", {
  skip_on_cran()
  table <- dplyr::tibble(
    subject_id = 1:6,
    prior_history = c(1, 8, Inf, -Inf, 20, NA),
    date_infection = as.Date(c(
      "2020-01-01", NA, "2020-12-21", "2020-08-01", "2025-01-01", "2020-01-18"
    ))
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 10
  )
  cdm <- CDMConnector::insertTable(cdm = cdm, name = "table", table = table)
  table <- cdm$table %>%
    addCategories(
      variable = "prior_history", categories = list(
        "prior_group" = list(c(1, 10), c(11, Inf))
      ), missingCategoryValue = "None", overlap = FALSE
    ) %>%
    addCategories(
      variable = "date_infection", categories = list(
        "period1" = list(
          as.Date(c("2019-01-01", "2022-12-31")),
          as.Date(c("2023-01-01", "2028-12-31"))
        )
      ), missingCategoryValue = "None", overlap = FALSE
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$subject_id)
  # check inf worked
  expect_true("prior_group" %in% colnames(table))
  expect_true(is.na(table$prior_group[6]))
  xx <- table$prior_group[!is.na(table$prior_group)]
  expect_true(all(xx == c("1 to 10", "1 to 10", "11 or above", "None", "11 or above")))
  # check date worked
  expect_true("period1" %in% colnames(table))
  expect_true(is.na(table$period1[2]))
  xx <- table$period1[!is.na(table$period1)]
  expect_true(all(xx == c(
    "2019-01-01 to 2019-01-01", "2019-01-01 to 2019-01-01",
    "2019-01-01 to 2019-01-01", "2023-01-01 to 2023-01-01",
    "2019-01-01 to 2019-01-01"
  )))
  mockDisconnect(cdm)
})
