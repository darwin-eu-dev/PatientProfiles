test_that("check input length and type for each of the arguments", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5
    )

  expect_error(addAge("cdm$cohort1", cdm))

  expect_error(addAge(cdm$cohort1, "cdm"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "subject_id"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date", ageDefaultMonth = "1"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date", ageDefaultDay = "1"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date", ageImposeMonth = "TRUE"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date", ageImposeDay = "TRUE"))

  expect_error(addAge(cdm$cohort1, cdm, indexDate = "cohort_start_date", tablePrefix = 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("check condition_occurrence and cohort1 work", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5
    )

  expect_true(typeof(cdm$cohort1 %>% addAge(cdm) %>% dplyr::collect()) == "list")
  expect_true("age" %in% colnames(cdm$cohort1 %>% addAge(cdm)))

  expect_true(typeof(cdm$condition_occurrence %>% addAge(cdm, indexDate = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("age" %in% colnames(cdm$condition_occurrence %>% addAge(cdm, indexDate = "condition_start_date")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("age at cohort entry, missing year/month/day of birth", {
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
    year_of_birth = c(2000, NA, 2000),
    month_of_birth = c(03, 07, NA),
    day_of_birth = c(NA, 02, 01)
  )

  cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)

  result <- addAge(
    x = cdm$cohort1, cdm = cdm, ageImposeMonth = FALSE, ageImposeDay = FALSE,
    ageDefaultMonth = 4, ageDefaultDay = 4
  ) %>% dplyr::collect()

  expect_true(all(c(colnames(cohort1), "age") %in% colnames(result)))

  expect_equal(nrow(result), 3)

  expect_true(identical(result$age, c(9, 9, NA)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("age at cohort end, no missing, check age computation", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2002-11-30"), as.Date("2002-12-02")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8507"),
    year_of_birth = c(2001, 2001),
    month_of_birth = c(12, 12),
    day_of_birth = c(01, 01)
  )

  cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)

  # check if exact age is computed, ie, dob 2000-01-01, target date 2000-12-01  --> age 0
  # dob 2000-01-01, target date 2001-01-02  --> age 1
  result <- addAge(x = cdm[["cohort1"]], cdm = cdm, ageImposeMonth = FALSE, ageImposeDay = FALSE) %>% dplyr::collect()
  expect_true(identical(result$age, c(0, 1)))
})

test_that("check expected errors", {
  cdm <- mockPatientProfiles()

  expect_error(result <- addAge(cdm = "a"))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm, ageImposeDay = 1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm, ageImposeMonth = 1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm, indexDate = "date"))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm, ageDefaultMonth = 1.1))
  expect_error(result <- addAge(x = cdm[["cohort1"]], cdm = cdm, ageDefaultDay = 1.1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("multiple cohortIds, check age at cohort end", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "2", "3"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2009-12-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
    )
  )

  person <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8532", "8507"),
    year_of_birth = c(2000, 2000, NA),
    month_of_birth = c(NA, 01, 01),
    day_of_birth = c(01, 01, 01)
  )
  cdm <- mockPatientProfiles(person = person, cohort1 = cohort1)

  result <- addAge(x = cdm[["cohort1"]], cdm = cdm, indexDate = "cohort_end_date") %>% dplyr::collect()

  expect_true(identical(result$subject_id, c("1", "2", "3")))
  expect_true(identical(result$age, c(15, 13, NA)))

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

  expect_true(typeof(cdm$cohort1 %>% addCategories(cdm, "age", categories) %>% dplyr::collect()) == "list")
  expect_true("age_group" %in% colnames(cdm$cohort1 %>% addCategories(cdm, "age", categories)))

  expect_true(typeof(cdm$condition_occurrence %>% addCategories(cdm, "age", categories) %>% dplyr::collect()) == "list")
  expect_true("age_group" %in% colnames(cdm$condition_occurrence %>% addCategories(cdm, "age", categories)))

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
    x, cdm, "age", list("age_group" = list(c(1, 20), c(21, 30), c(31, 40)))
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(all(result1$age_group == c("1 to 20", "21 to 30", "31 to 40")))

  # change the order of ageGroup provided, result should be the same
  result2 <- addCategories(
    x, cdm, "age", list("age_group" = list(c(21, 30), c(1, 20), c(31, 40)))
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
    cdm, "age",
    list("age_group" = list(c(1, 22), c(19, 30), c(31, 40)))
  ))

  # throw error if length of vector in agegroup is not 2
  expect_error(addCategories(
    cdm$cohort1,
    cdm, "age",
    list("age_group" = list(c(1, 2, 3)))
  ))

  # if x does not have "age" column, it has to be in cdm
  expect_error(addCategories(
    cdm$cohort2,
    cdm,  "age",
    list("age_group" = list(c(1, 2)))
  ))

})

test_that("different name", {
  cdm <-
    mockPatientProfiles(
      seed = 1,
      patient_size = 5
    )
  cdm$cohort1 <- cdm$cohort1 %>% addAge(cdm, ageName = "working_age")

  expect_true("working_age" %in% colnames(cdm$cohort1))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
