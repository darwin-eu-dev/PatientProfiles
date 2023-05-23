
test_that("addCategories, functionality", {
  cdm <- mockPatientProfiles(seed = 11, patient_size = 10)
  agegroup <- cdm$cohort1 %>%
    addAge(cdm) %>%
    addCategories(cdm,
                  variable = "age",
                  categories = list("age_group" = list(c(0, 40), c(41, 120)))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date)

  agegroup_overlap <- cdm$cohort1 %>%
    addAge(cdm) %>%
    addCategories(cdm,
                  variable = "age",
                  categories = list("age_group" = list(c(0, 55), c(50, 120))),
                  overlap = TRUE
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(agegroup %>%
                    dplyr::select(age_group) %>%
                    dplyr::pull() ==
                    c( "41 to 120", "41 to 120", "41 to 120", "41 to 120")))

  expect_true(all(agegroup_overlap %>%
                    dplyr::select(age_group) %>%
                    dplyr::pull() ==
                    c( "0 to 55&&50 to 120", "0 to 55&&50 to 120", "50 to 120", "50 to 120")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
