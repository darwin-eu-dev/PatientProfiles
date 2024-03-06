test_that("expected output", {

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 2, recordPerson = 2)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm_local,
                                   schema = "main")

  timing1 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = TRUE,
                                    timing = c("min", "q25",
                                               "median","q75",
                                               "max"))

  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing1))

  expect_true(all(timing1$estimate_name |> unique() %in%
                    c("min", "q25","median","q75","max","count")))

  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() ==
      unique(timing1 |>
               dplyr::filter(group_level == "cohort_1 and cohort_1" & estimate_name == "count") |>
               dplyr::pull(estimate_value))
  )


  timing2 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = FALSE,
                                    timing = c("min", "q25",
                                               "median","q75",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing2))

  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() !=
      timing2 |> dplyr::filter(group_level == "cohort_1 and cohort_1" & variable_name == "number records") |>
               dplyr::pull(estimate_value)
  )
  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() ==
      timing2 |> dplyr::filter(group_level == "cohort_1 and cohort_1" & variable_name == "number subjects") |>
      dplyr::pull(estimate_value)
  )

  timing3 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = TRUE,
                                    timing = c("min",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(timing3))

  expect_true(all(timing3$estimate_name |> unique() %in%
                    c("min","max","count")))

  CDMConnector::cdm_disconnect(cdm)

})
