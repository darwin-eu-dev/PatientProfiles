test_that("check input length and type for each of the arguments", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5
    )

  expect_error(addCategories("cdm$cohort1"))

  expect_error(addCategories(cdm$cohort1, "cdm"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check condition_occurrence and cohort1 work", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 3
    )

  cdm$cohort1 <- cdm$cohort1 %>% addAge(cdm)
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addAge(cdm, indexDate = "condition_start_date")
  categories <- list("age_group" = list(c(0,20)))

  expect_true(typeof(cdm$cohort1 %>% addCategories("age", categories) %>% dplyr::collect()) == "list")
  expect_true("age_group" %in% colnames(cdm$cohort1 %>% addCategories("age", categories)))

  expect_true(typeof(cdm$condition_occurrence %>% addCategories("age", categories) %>% dplyr::collect()) == "list")
  expect_true("age_group" %in% colnames(cdm$condition_occurrence %>% addCategories("age", categories)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("NULL age group name, but given age groups, age not in table", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)

  x <- cdm$cohort1 %>%
    addAge(cdm)

  result1 <- addCategories(
    x, "age", list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(all(result1$age_group == c("1 to 20", "21 to 30", "31 to 40")))

  # change the order of ageGroup provided, result should be the same
  result2 <- addCategories(
    x, "age", list("age_group" = list(c(21, 30), c(1, 20), c(31, 40)))
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  result3 <- cdm$cohort1 %>%
    addAge(cdm, ageGroup = list(c(1, 20), c(21, 30), c(31, 40))) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$age)

  expect_true(identical(result1, result2))
  expect_true(identical(result1, result3))
  expect_true(identical(result3, result2))
})

test_that("throw errors", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8507"),
    year_of_birth = c(1980, 1970, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)

  cdm$cohort1 <- cdm$cohort1 %>% addAge(cdm)

  # error if overlapping ageGroyp
  expect_error(addCategories(
    cdm$cohort1,
    "age",
    list("age_group" = list(c(1, 22), c(19, 30), c(31, 40)))
  ))

  # throw error if length of vector in agegroup is not 2
  expect_error(addCategories(
    cdm$cohort1,
    "age",
    list("age_group" = list(c(1, 2, 3)))
  ))

  # if x does not have "age" column, it has to be in cdm
  expect_error(addCategories(
    cdm$cohort2,
    "age",
    list("age_group" = list(c(1, 2)))
  ))

})
