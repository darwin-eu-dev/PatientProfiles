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

  cdm <- mockCohortProfiles(person = person, cohort1 = cohort1)

  result1 <- addAgeGroup(
    x = cdm$cohort1, ageGroup = list(c(1, 20), c(21, 30), c(31, 40)), cdm = cdm
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(all(result1$ageGroupNames == c("1;20", "21;30", "31;40")))

  # change the order of ageGroup provided, result should be the same
  result2 <- addAgeGroup(
    x = cdm$cohort1, ageGroup = list(c(21, 30), c(1, 20), c(31, 40)), cdm = cdm
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(identical(result1, result2))

  #if provided ageGroupNames as follow, should be the same as result2
  result3 <- addAgeGroup(
    x = cdm$cohort1, ageGroup = list(c(1, 20), c(21, 30), c(31, 40)),
    ageGroupNames = c("1;20", "21;30", "31;40"), cdm = cdm
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(identical(result3, result2))

  # allow vector of length 2 as ageGroup, same output as if input as list
  result1 <- addAgeGroup(
    x = cdm$cohort1, ageGroup = c(1, 20), cdm = cdm
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  result2 <- addAgeGroup(
    x = cdm$cohort1, ageGroup = list(c(1, 20)), cdm = cdm
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(age)

  expect_true(identical(result1, result2))
})



test_that("when NULL ageGRoup provided, ageGroupNames is 0;150 if not provided,
          or ageGroupNames can be defined by user, but cannot be more than 1 name", {
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

  cdm <- mockCohortProfiles(person = person, cohort1 = cohort1)

  # if NULL ageGroup, 1 ageGroupNames provided, use it in ageGroupNames column in output
  result <- addAgeGroup(
    x = cdm$cohort1,
    ageGroupNames = c("1"), cdm = cdm
  ) %>% dplyr::collect()

  expect_true(unique(result$ageGroupNames) == "1")


  # if NULL for both ageGroup and ageGroupNames, use 0;150 for ageGroup and ageGroupNames

  result <- addAgeGroup(
    x = cdm$cohort1, cdm = cdm
  ) %>% dplyr::collect()

  expect_true(unique(result$ageGroupNames) == "0;150")


  # if NULL ageGroup, but more than 1 ageGroupNames provided, report error
  expect_error(addAgeGroup(
    x = cdm$cohort1, cdm = cdm,
    ageGroupNames = c("1", "2")
  ))
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

  cdm <- mockCohortProfiles(person = person, cohort1 = cohort1)

  # error if overlapping ageGroyp
  expect_error(addAgeGroup(
    x = cdm$cohort1,
    ageGroup = list(c(1, 22), c(19, 30), c(31, 40)),
    ageGroupNames = c("1"), cdm = cdm
  ))

  # error if length of ageGroup provided is different from
  # length of ageGroupNames provided
  expect_error(addAgeGroup(
    x = cdm$cohort1,
    ageGroup = list(c(1, 20), c(21, 30), c(31, 40)),
    ageGroupNames = c("1"), cdm = cdm
  ))

  expect_error(addAgeGroup(
    x = cdm$cohort1,
    ageGroup = list(c(1, 20)),
    ageGroupNames = c("1", "2"), cdm = cdm
  ))

  # throw error if length of vector in agegroup is not 2
  expect_error(addAgeGroup(
    x = cdm$cohort1,
    ageGroup = list(c(1, 20, 30)),
    cdm = cdm
  ))

  expect_error(addAgeGroup(
    x = cdm$cohort1,
    ageGroup = list(c(1, 2), c(1, 20, 30)),
    cdm = cdm
  ))

  # if x does not have "age" column, it has to be in cdm
  expect_error(addAgeGroup(
    x = cohort1,
    ageGroup = list(c(1, 2), c(3, 20)),
    cdm = cdm
  ))


  # but if age in x columns, does not need to be in cdm, should not have error
  cohort1$age <- c(1, 2, 3)
  # throw error if when age is in x columns, function still throw error
  expect_error(expect_error(addAgeGroup(
    x = cohort1,
    ageGroup = list(c(1, 2), c(3, 20)),
    cdm = cdm
  )))
})
