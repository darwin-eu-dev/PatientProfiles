
test_that("test checkCategory with length 1 ", {
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = c(1980, 1950),
    month_of_birth = 01,
    day_of_birth = 01,
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(
      c("2020-01-01", "2020-01-15")
    ),
    cohort_end_date = as.Date(
      c("2020-01-01", "2020-01-15")
    )
  )

  op <- dplyr::tibble(
    person_id = 1:2,
    observation_period_id = 1:2,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2030-01-01"),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    connectionDetails = connectionDetails, cohort1 = cohort1, person = person,
    observation_period = op
  )

  categories <- list("age_group" = list(c(0, 69), c(70)))

  a <- cdm$cohort1 %>%
    addAge(indexDate = "cohort_start_date") %>%
    addCategories("age", categories) %>%
    dplyr::collect()
  expect_true(a[a$subject_id == 2, ]$age_group == "70 to 70")

  # check invalid groups

  categories <- list("age_group" = list(c(69, 0), c(70)))

  expect_error(cdm$cohort1 %>% addAge(indexDate = "cohort_start_date") %>%
    addCategories("age", categories))
})

test_that(" test checkNewName renames duplicate column names in addInObservation  ", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    ),
    flag = c(0, 0)
  )
  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = c(1980, 1950),
    month_of_birth = 01,
    day_of_birth = 01,
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  op <- dplyr::tibble(
    person_id = 1:2,
    observation_period_id = 1:2,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2030-01-01"),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    connectionDetails = connectionDetails, cohort1 = cohort1, person = person,
    observation_period = op
  )

  expect_warning(x <- addInObservation(cdm$cohort1, name = "flag"))
  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag"
  ) == colnames(x)))
  y <- addInObservation(cdm$cohort1, name = "flag_new")
  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag", "flag_new"
  ) == colnames(y)))

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    ),
    flag = c(0, 0),
    flag_1 = c(0, 0)
  )
  cdm <- mockPatientProfiles(
    connectionDetails = connectionDetails, cohort1 = cohort1, person = person,
    observation_period = op
  )

  expect_warning(x <- addInObservation(cdm$cohort1, name = "flag"))
  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag", "flag_1"
  ) == colnames(x)))
  expect_true(x |> dplyr::pull("flag") |> unique() == 1)
  expect_true(x |> dplyr::pull("flag_1") |> unique() == 0)
  y <- addInObservation(cdm$cohort1, name = "flag_new")
  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag", "flag_1", "flag_new"
  ) == colnames(y)))
  expect_true(y |> dplyr::pull("flag") |> unique() == 0)
  expect_true(y |> dplyr::pull("flag_new") |> unique() == 1)
  expect_true(y |> dplyr::pull("flag_1") |> unique() == 0)

})

test_that(" test checkWindow in addIntersect", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 2)

  expect_error(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(-NA, 0)), value = "date"))
  expect_error(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(-365, 0, 1)), value = "date"))
  expect_warning(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(-365), -c(0), -c(30)), value = "date"))

  expect_error(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(30, -365)), value = "date"))
  expect_error(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(Inf, Inf)), value = "date"))
  expect_error(cdm$cohort1 %>% addIntersect(tableName = "cohort2", window = list(c(-Inf, -Inf)), value = "date"))
})

test_that("test checkSnakeCase", {
  expect_true(checkSnakeCase("Age") == "age")
  expect_true(checkSnakeCase("age groups") == "age_groups")
  expect_true(checkSnakeCase("new-var") == "new_var")
  expect_true(checkSnakeCase("this_is_snake") == "this_is_snake")
  expect_true(checkSnakeCase("this_Is_Not_Snake") == "this_is_not_snake")
  expect_true(checkSnakeCase("thisIsNotSnake") == "thisisnotsnake")
  expect_true(checkSnakeCase("this-is-not-snake") == "this_is_not_snake")
  expect_true(checkSnakeCase("this_is_alm@st_snake") == "this_is_alm_st_snake")
})

test_that("check window", {
  window <- list("short" = c(0, 9), c(10, 20), c(20, 35), "long" = c(-50, 10))
  windowName <- checkWindow(window)
  expect_true("tbl" %in% class(windowName))
  expect_true(all(names(windowName) == c("lower", "upper", "window_name")))
  expect_true(all(windowName$lower == c(0, 10, 20, -50)))
  expect_true(all(windowName$upper == c(9, 20, 35, 10)))
  expect_true(all(windowName$window_name == c("short", "10_to_20", "20_to_35", "long")))
})

test_that("checkAgeGroup", {
  ageGroup <- list(list(c(0, 69), c(70)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_1[[1]] == c(0, 69)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_1[[2]] == c(70)))

  ageGroup <- list("age_group" = list(c(0, 40), c(41, 120)), list(c(0, 20)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_2[[1]] == c(0, 20)))
})

test_that("checkNameStyle", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 1, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26"
      )
    ),
  )

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = c(1980, 1950),
    month_of_birth = 01,
    day_of_birth = 01,
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  op <- dplyr::tibble(
    person_id = 1:2,
    observation_period_id = 1:2,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2030-01-01"),
    period_type_concept_id = 0
  )

  cdm <- mockPatientProfiles(
    connectionDetails = connectionDetails, cohort1 = cohort1, person = person,
    observation_period = op, cohort2 = cohort2
  )

  expect_true(all(c("count_all", "flag_all") %in% colnames(cdm$cohort1 %>% addIntersect(
    tableName = "cohort2", value = c("flag", "count"),
    nameStyle = "{value}_{id_name}"
  ))))
})

test_that("test assertNameStyle", {
  expect_error(
    assertNameStyle("my_name", values = list(
      "variable1" = 1, "variable2" = c("a", "b", "c")
    ))
  )

  expect_no_error(
    assertNameStyle("my_name_{variable2}", values = list(
      "variable1" = 1, "variable2" = c("a", "b", "c")
    ))
  )

  expect_error(
    assertNameStyle("my_name_{variable2}", values = list(
      "variable1" = c(1, 2), "variable2" = c("a", "b", "c")
    ))
  )

  expect_no_error(
    assertNameStyle("my_name_{variable1}_{variable2}", values = list(
      "variable1" = c(1, 2), "variable2" = c("a", "b", "c")
    ))
  )
})
