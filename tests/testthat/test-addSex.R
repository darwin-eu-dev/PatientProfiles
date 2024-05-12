test_that("addSex, check imput length and type", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_error(addSex("cdm$cohort1"))
  expect_error(addSex(cdm$drug_strength))
  expect_error(addSex(cdm$cohort1, name = 2))
  expect_error(addSex(cdm$cohort1, name = c("name1", "name2")))
  expect_error(addSex(cdm))
  mockDisconnect(cdm = cdm)
})

test_that("addSex, works in both cohort and condition tables", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  cdm$cohort1 <- cdm$cohort1 %>% addSex()
  cdm$condition_occurrence <- cdm$condition_occurrence %>% addSex()
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true(all(cdm$cohort1 %>% dplyr::pull("sex") %in% c("Female", "Male")))
  expect_true("sex" %in% colnames(cdm$condition_occurrence))
  expect_true(all(cdm$condition_occurrence %>% dplyr::pull("sex") %in% c("Female", "Male")))
  mockDisconnect(cdm = cdm)
})

test_that("addSex, desired result for all parameters", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    person = dplyr::tibble(
      person_id = 1:5,
      gender_concept_id = c(8507, 8532, 0, NA, 123456789),
      year_of_birth = 2000,
      race_concept_id = 0,
      ethnicity_concept_id = 0
    ),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1,
      subject_id = 1:5,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01")
    )
  )
  cdm$cohort1 <- cdm$cohort1 %>% addSex()
  expect_true("sex" %in% colnames(cdm$cohort1))
  expect_true(all(
    cdm$cohort1 |>
      dplyr::collect() |>
      dplyr::arrange(.data$subject_id) |>
      dplyr::pull("sex") == c("Male", "Female", "None", "None", "None")
  ))
  expect_warning(cdm$cohort1 <- cdm$cohort1 |> addSex())
  cdm$cohort1 <- cdm$cohort1 |>
    addSex(sexName = "gender", missingSexValue = "Missing")
  expect_true("gender" %in% colnames(cdm$cohort1))
  expect_true(all(
    cdm$cohort1 |>
      dplyr::collect() |>
      dplyr::arrange(.data$subject_id) |>
      dplyr::pull("gender") ==
      c("Male", "Female", "Missing", "Missing", "Missing")
  ))
  mockDisconnect(cdm = cdm)
})
