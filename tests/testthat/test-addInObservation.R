test_that("addInObservation, input length and type", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_error(addInObservation(2))
  expect_error(addInObservation(cdm$concept_ancestor))
  expect_error(addInObservation(cdm$cohort1, indexDate = 3))
  expect_error(addInObservation(cdm$cohort1, indexDate = "2002-01-02"))
  expect_error(addInObservation(cdm$cohort1, indexDate = c("cohort", "cohort_end")))
  expect_error(addInObservation(cdm$cohort2, nameStyle = 3))
  expect_error(addInObservation(cdm$cohort2, nameStyle = c("name1", "name2")))

  mockDisconnect(cdm = cdm)
})

test_that("addInObservation, cohort and condition_occurrence", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

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

  mockDisconnect(cdm = cdm)
})

test_that("addInObservation, parameters", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  result1 <- addInObservation(cdm$condition_occurrence, indexDate = "condition_end_date", nameStyle = "observ")
  expect_true("observ" %in% colnames(result1))
  expect_false("in_observation" %in% colnames(result1))

  expect_true(all(result1 %>% dplyr::collect() |> dplyr::arrange(condition_occurrence_id, condition_start_date) %>% dplyr::select(observ) %>% dplyr::pull() == 1))

  mockDisconnect(cdm = cdm)
})

test_that("addInObservation, window", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = as.Date(c("2020-01-01", "2015-05-12")),
      cohort_end_date = cohort_start_date
    ),
    observation_period = dplyr::tibble(
      person_id = 1,
      observation_period_start_date = as.Date("2010-01-01"),
      observation_period_end_date = as.Date("2050-12-31"),
      observation_period_id = 1L,
      period_type_concept_id = 0L
    )
  )

  # both true
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1957, 11322), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1957, 11322), completeInterval = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))

  # just first observation
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1958, 11322), completeInterval = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1958, 11322), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(0, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1957, 11323), completeInterval = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-1957, 11323), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 0)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3652, 11322), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(0, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3652, 11322), completeInterval = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3653, 11322), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(0, 0)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3652, 11323), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(0, 0)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3652, 13017), completeInterval = T) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(0, 0)
  ))
  expect_true(all(
    cdm$cohort1 |>
      addInObservation(window = c(-3652, 13017), completeInterval = F) |>
      dplyr::collect() |>
      dplyr::arrange(cohort_start_date) |>
      dplyr::pull(in_observation) == c(1, 1)
  ))

  mockDisconnect(cdm = cdm)
})

test_that("query gives same result as main function", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  # we should get the same results if compute was internal or not
  result_1 <- cdm$cohort1 %>%
    addInObservation() %>%
    dplyr::collect()
  result_2 <- cdm$cohort1 %>%
    addInObservationQuery() |>
    dplyr::collect()
  expect_equal(result_1, result_2)

  # check no tables are created along the way with query
  start_tables <- CDMConnector::listSourceTables(cdm)
  cdm$cohort1 %>%
    addInObservationQuery()
  end_tables <- CDMConnector::listSourceTables(cdm)
  expect_equal(start_tables, end_tables)

  mockDisconnect(cdm)
})
