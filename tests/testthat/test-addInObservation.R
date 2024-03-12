test_that("addInObservation, input length and type", {
  cdm <- mockPatientProfiles(connectionDetails)
  expect_error(addInObservation(2))
  expect_warning(addInObservation(cdm$cohort2, "cdm"))
  expect_error(addInObservation(cdm$concept_ancestor))
  expect_error(addInObservation(cdm$cohort1, indexDate = 3))
  expect_error(addInObservation(cdm$cohort1, indexDate = "2002-01-02"))
  expect_error(addInObservation(cdm$cohort1, indexDate = c("cohort", "cohort_end")))
  expect_error(addInObservation(cdm$cohort2, name = 3))
  expect_error(addInObservation(cdm$cohort2, name = c("name1", "name2")))
})

test_that("addInObservation, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(connectionDetails)

  result1 <- addInObservation(cdm$cohort1)
  expect_true("in_observation" %in% colnames(result1))
  expect_true(all(
    result1 %>%
      dplyr::collect() %>%
      dplyr::arrange(cohort_definition_id, cohort_start_date) %>%
      dplyr::select(in_observation) %>%
      dplyr::pull() == c(1, 1)
  ))
  result2 <- addInObservation(cdm$cohort2)


  expect_true("in_observation" %in% colnames(result2))
  expect_true(all(result2 %>% dplyr::collect() |> dplyr::arrange(cohort_definition_id, cohort_start_date) %>% dplyr::select(in_observation) %>% dplyr::pull() == 1))

  result3 <- addInObservation(cdm$cohort1 %>% dplyr::rename(person_id = subject_id))
  expect_true("in_observation" %in% colnames(result3))
  expect_true(all(result1 %>% dplyr::select(in_observation) %>% dplyr::pull() == result3 %>%
    dplyr::select(in_observation) %>%
    dplyr::pull()))
  result4 <- addInObservation(cdm$condition_occurrence, indexDate = "condition_start_date")
  expect_true("in_observation" %in% colnames(result4))
  expect_true(all(result4 %>% dplyr::collect() |> dplyr::arrange(condition_occurrence_id, condition_start_date) %>% dplyr::select(in_observation) %>% dplyr::pull() == 1))
})

test_that("addInObservation, parameters", {
  cdm <- mockPatientProfiles(connectionDetails)

  result1 <- addInObservation(cdm$condition_occurrence, indexDate = "condition_end_date", name = "observ")
  expect_true("observ" %in% colnames(result1))
  expect_false("in_observation" %in% colnames(result1))

  expect_true(all(result1 %>% dplyr::collect() |> dplyr::arrange(condition_occurrence_id, condition_start_date) %>% dplyr::select(observ) %>% dplyr::pull() == 1))
})

test_that("addInObservation, window", {

  cdm <- mockPatientProfiles()

  expect_true(all(
    cdm$cohort1 |> addInObservation(window = c(-5055, 5046)) |> dplyr::pull(in_observation) == c(0, 1)
  ))
  expect_true(all(
    cdm$cohort1 |> addInObservation(window = c(-5054, 5046)) |> dplyr::pull(in_observation) == c(1, 1)
  ))

  expect_true(all(
    cdm$cohort1 |> addInObservation(window = c(-5055, 30042)) |> dplyr::pull(in_observation) == c(0, 0)
  ))
  expect_true(all(
    cdm$cohort1 |> addInObservation(window = c(-5055, 30042), completeInterval = F) |> dplyr::pull(in_observation) == c(0, 1)
  ))

}
)
