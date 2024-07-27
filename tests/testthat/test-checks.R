test_that("test checkCategory with length 1 ", {
  skip_on_cran()
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
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    person = person,
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

  expect_error(checkX(dplyr::tibble()))

  expect_error(checkX(cdm$person |> dplyr::mutate("subject_id" = 1)))

  expect_error(checkX(cdm$person |> dplyr::select(-"person_id")))

  expect_error(checkCdm(list()))

  expect_warning(checkValue(
    "flag", cdm$person |> dplyr::mutate("flag" = 1), "person"
  ))

  expect_error(checkCohortNames(dplyr::tibble()))

  expect_error(checkExclude(1))

  expect_error(checkTable(1))

  expect_error(checkStrata(1))

  expect_error(checkStrata(list("sex"), dplyr::tibble()))

  expect_error(checkVariablesFunctions(list("sex"), list("count", "mean")))

  expect_error(checkVariablesFunctions(list("a" = "sex"), list("b" = "count")))

  expect_identical(
    checkVariablesFunctions(list(), list()),
    rep(list(character()), 4) |>
      rlang::set_names(c("variable_name", "estimate_name", "variable_type", "estimate_type")) |>
      dplyr::as_tibble()
  )

  expect_error(checkAgeGroup(list(c(-5, 0))))

  expect_error(checkWindow(c(0, 180)))

  expect_no_error(
    x <- checkVariablesFunctions(
      variables = list(c("age", "bin")),
      estimates = list(c("count", "mean")),
      table = dplyr::tibble("age" = 3, "bin" = 1)
    )
  )
  expect_true("count" %in% x$estimate_name[x$variable_name == "bin"])
  expect_false("count" %in% x$estimate_name[x$variable_name == "age"])

  expect_error(assertCharacter("ASD", minNumCharacter = 4))

  expect_error(assertList(list("ac"), class = "cdm"))

  expect_error(assertNumeric("asd"))

  expect_error(assertNumeric(5, min = 6))

  expect_error(assertNumeric(6, max = 5))

  expect_no_error(assertClass(NULL, class = "cdm", null = T))
  expect_error(assertClass(NULL, class = "cdm", null = F))

  expect_error(assertCharacter(NA_character_))

  expect_error(checkStrata(list(3)))

  expect_error(assertCharacter("sadv", named = T))

  expect_error(assertChoice(2, c("asd", "sad")))

  expect_error(assertLogical(1))

  mockDisconnect(cdm = cdm)
})

test_that(" test checkNewName renames duplicate column names in addInObservation  ", {
  skip_on_cran()
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-15")),
    cohort_end_date = as.Date(c("2020-01-01", "2020-01-15")),
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
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    person = person,
    observation_period = op
  )

  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag"
  ) == colnames(cdm$cohort1)))
  expect_warning(x <- cdm$cohort1 |> addInObservation(nameStyle = "flag"))

  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag"
  ) == colnames(x)))
  y <- addInObservation(cdm$cohort1, nameStyle = "flag_new")
  expect_true(all(c(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "flag", "flag_new"
  ) == colnames(y)))
})

test_that(" test checkWindow in addIntersect", {
  skip_on_cran()
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    seed = 11,
    numberIndividuals = 2
  )

  expect_error(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(-NA, 0)), value = "date"))
  expect_error(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(-365, 0, 1)), value = "date"))
  expect_warning(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(-365), -c(0), -c(30)), value = "date"))

  expect_error(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(30, -365)), value = "date"))
  expect_error(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(Inf, Inf)), value = "date"))
  expect_error(cdm$cohort1 %>% .addIntersect(tableName = "cohort2", window = list(c(-Inf, -Inf)), value = "date"))

  mockDisconnect(cdm = cdm)
})

test_that("test checkSnakeCase", {
  skip_on_cran()
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
  skip_on_cran()
  window <- list("short" = c(0, 9), c(10, 20), c(20, 35), "long" = c(-50, 10))
  windowCorrected <- checkWindow(window)
  expect_true("list" %in% class(windowCorrected))
  expect_true(all(lapply(windowCorrected, function(x) {
    x[1]
  }) |> unlist() == c(0, 10, 20, -50)))
  expect_true(all(lapply(windowCorrected, function(x) {
    x[2]
  }) |> unlist() == c(9, 20, 35, 10)))
  expect_true(all(names(windowCorrected) == c("short", "10_to_20", "20_to_35", "long")))
})

test_that("checkAgeGroup", {
  skip_on_cran()
  ageGroup <- list(list(c(0, 69), c(70)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_1[[1]] == c(0, 69)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_1[[2]] == c(70)))

  ageGroup <- list("age_group" = list(c(0, 40), c(41, 120)), list(c(0, 20)))

  expect_true(all(checkAgeGroup(ageGroup)$age_group_2[[1]] == c(0, 20)))
})

test_that("checkNameStyle", {
  skip_on_cran()
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-15")),
    cohort_end_date = cohort_start_date
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 1, 2),
    cohort_start_date = as.Date(c("2020-01-15", "2020-01-25", "2020-01-26")),
    cohort_end_date = cohort_start_date,
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
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    person = person,
    observation_period = op,
    cohort2 = cohort2
  )

  expect_true(all(c("count_all", "flag_all") %in% colnames(cdm$cohort1 %>% .addIntersect(
    tableName = "cohort2", value = c("flag", "count"),
    nameStyle = "{value}_{id_name}"
  ))))
})

test_that("test assertNameStyle", {
  skip_on_cran()
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

test_that("test warnOverwriteColumns", {
  skip_on_cran()
  # no glue expression
  x <- dplyr::tibble("my_columns" = character(), "no_column" = character())
  expect_message(
    y <- warnOverwriteColumns(x, "no_column")
  )
  expect_identical(colnames(y), "my_columns")
  expect_no_message(
    y <- warnOverwriteColumns(x, c("asdfd", "safvf"))
  )
  expect_identical(x, y)

  # glue expression
  expect_no_message(warnOverwriteColumns(
    x, "column_{a}_{b}", list(a = c("abcd", "defg"), b = "hi", x = "a")
  ))
  x <- dplyr::tibble(
    my_col = character(), column_abcd_hi = character(),
    column_sadf_ha = character(), column_defg_hu = character()
  )
  expect_message(y <- warnOverwriteColumns(
    x,
    nameStyle = "column_{a}_{b}",
    values = list(a = c("abcd", "defg"), b = c("hi", "ha", "hu"), x = "a")
  ))
  yy <- dplyr::tibble(my_col = character(), column_sadf_ha = character())
  expect_identical(y, yy)
})
