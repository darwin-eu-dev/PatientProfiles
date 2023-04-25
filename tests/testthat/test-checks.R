test_that("test checkX: subject_id and person_id", {
cohort1 <- dplyr::tibble(
  cohort_definition_id = c(1, 1),
  subject_id = c(1, 1),
  person_id = c(1, 1),
  cohort_start_date = as.Date(
    c("2020-01-01", "2020-01-15") ),
  cohort_end_date = as.Date(
    c("2020-01-01","2020-01-15")
  )
)
cdm <- mockPatientProfiles(cohort1 = cohort1)

expect_error(cdm$cohort1 %>% addDemographics(cdm))

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


test_that("test checkCategory with length 1 ", {
  person <- tibble::tibble(
    person_id = c(1, 2),
    gender_concept_id = 1,
    year_of_birth = c(1980, 1950),
    month_of_birth = 01,
    day_of_birth = 01
  )

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(
      c("2020-01-01", "2020-01-15") ),
    cohort_end_date = as.Date(
      c("2020-01-01","2020-01-15")
    )
  )

  cdm <- mockPatientProfiles(cohort1 = cohort1,person = person )

  categories <- list("age_group" = list(c(0, 69), c(70)))

    a <- cdm$cohort1 %>% addAge(cdm, indexDate = "cohort_start_date") %>%
                                          addCategories(cdm, "age", categories) %>%
      dplyr::collect()
    expect_true(a[a$subject_id==2,]$age_group == "70 to 70")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


test_that("test #checkValue: multiple value  ", {

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
  flag=c(1, 1, 1),
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

cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

expect_warning(cdm$cohort1 %>% addIntersect(cdm = cdm, tableName = "cohort2", value = c("flag", "count")) )

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


test_that(" test checkNewName renames duplicate column names in addInObservation  " ,{
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
    flag = c(0,0)
  )
  cdm <- mockPatientProfiles(cohort1 = cohort1)

  expect_warning(addInObservation(cdm$cohort1,cdm, name="flag"))
  expect_true(all(c("flag", "flag_new") %in% colnames(addInObservation(cdm$cohort1,cdm, name="flag_new"))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that(" test checkWindow in addIntersect" ,{
  cdm <- mockPatientProfiles(seed = 11, patient_size = 2)

   expect_error(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(-NA, 0)), value = "date"))
   expect_error(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(-365, 0, 1)), value = "date"))
   expect_warning(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(-365),-c(0),-c(30)), value = "date"))

   expect_error(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(30,-365)), value = "date"))
   expect_error(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(Inf,Inf)), value = "date"))
   expect_error(cdm$cohort1 %>% addIntersect( cdm,tableName = "cohort2",window = list(c(-Inf,-Inf)), value = "date"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("test checkSnakeCase", {
  expect_error(checkSnakeCase("Age"))
  expect_error(checkSnakeCase("age groups"))
  expect_error(checkSnakeCase("new-var"))
  checkSnakeCase("this_is_snake")
  expect_error(checkSnakeCase("this_Is_Not_Snake"))
  expect_error(checkSnakeCase("thsiIsNotSnake"))
  expect_error(checkSnakeCase("this-is-not-snake"))
  expect_error(checkSnakeCase("this_is_alm@st_snake"))

})

test_that("check window", {
  window <- list("short" = c(0,9), c(10, 20), c(20, 35), "long" = c(-50, 10))
  windowName <- checkWindow(window)
  expect_true("tbl" %in% class(windowName))
  expect_true(all(names(windowName) == c("lower", "upper", "window_name")))
  expect_true(all(windowName$lower == c(0, 10, 20, -50)))
  expect_true(all(windowName$upper == c(9, 20, 35, 10)))
  expect_true(all(windowName$window_name == c("short", "10_to_20", "20_to_35", "long")))
})
