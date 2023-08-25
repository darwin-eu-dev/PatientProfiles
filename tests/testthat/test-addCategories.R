
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
    )
  ))
})
