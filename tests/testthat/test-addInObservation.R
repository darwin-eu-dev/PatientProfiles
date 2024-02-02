test_that("addInObservation, input length and type", {
  cdm <- mockPatientProfiles(connectionDetails)
  expect_error(addInObservation(2, cdm))
  expect_warning(addInObservation(cdm$cohort2, "cdm"))
  expect_warning(addInObservation(cdm$concept_ancestor, cdm))
  expect_error(addInObservation(cdm$cohort1, cdm, indexDate = 3))
  expect_error(addInObservation(cdm$cohort1, cdm, indexDate = "2002-01-02"))
  expect_error(addInObservation(cdm$cohort1, cdm, indexDate = c("cohort", "cohort_end")))
  expect_error(addInObservation(cdm$cohort2, cdm, name = 3))
  expect_error(addInObservation(cdm$cohort2, cdm, name = c("name1", "name2")))
})

test_that("addInObservation, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(connectionDetails)

  result1 <- addInObservation(cdm$cohort1, cdm)
  expect_true("in_observation" %in% colnames(result1))
  expect_true(all(
    result1 %>%
      dplyr::collect() %>%
      dplyr::arrange(cohort_definition_id, cohort_start_date) %>%
      dplyr::select(in_observation) %>%
      dplyr::pull() == c(1, 1)
  ))
  result2 <- addInObservation(cdm$cohort2, cdm)


  expect_true("in_observation" %in% colnames(result2))
  expect_true(all(result2 %>% dplyr::arrange(cohort_definition_id, cohort_start_date) %>% dplyr::select(in_observation) %>% dplyr::pull() == 1))

  result3 <- addInObservation(cdm$cohort1 %>% dplyr::rename(person_id = subject_id), cdm)
  expect_true("in_observation" %in% colnames(result3))
  expect_true(all(result1 %>% dplyr::select(in_observation) %>% dplyr::pull() == result3 %>%
    dplyr::select(in_observation) %>%
    dplyr::pull()))
  result4 <- addInObservation(cdm$condition_occurrence, cdm, indexDate = "condition_start_date")
  expect_true("in_observation" %in% colnames(result4))
  expect_true(all(result4 %>% dplyr::arrange(condition_occurrence_id, condition_start_date) %>% dplyr::select(in_observation) %>% dplyr::pull() == 1))
})

test_that("addInObservation, parameters", {
  cdm <- mockPatientProfiles(connectionDetails)

  result1 <- addInObservation(cdm$condition_occurrence, cdm, indexDate = "condition_end_date", name = "observ")
  expect_true("observ" %in% colnames(result1))
  expect_false("in_observation" %in% colnames(result1))

  expect_true(all(result1 %>% dplyr::arrange(condition_occurrence_id, condition_start_date) %>% dplyr::select(observ) %>% dplyr::pull() == 1))
})
