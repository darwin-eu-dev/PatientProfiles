test_that("expected output", {

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 2, recordPerson = 2)

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm_local,
                                   schema = "main")

  overlap1 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = TRUE,
                                    timing = c("min", "q25",
                                               "median","q75",
                                               "max"))

  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap1))

  expect_true(all(overlap1$estimate_name |> unique() %in%
                    c("min", "q25","median","q75","max","count")))

  res12 <- overlap1 |> dplyr::filter(group_level == "cohort_1 and cohort_2")
  res21 <- overlap1 |> dplyr::filter(group_level == "cohort_2 and cohort_1")

  expect_equal(sort(as.numeric(res12$estimate_value[res12$estimate_name %in% c("min", "q25", "median")])),
              sort(-as.numeric(res21$estimate_value[res12$estimate_name %in% c("max", "q75", "median")])))

  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() ==
      unique(overlap1 |>
               dplyr::filter(group_level == "cohort_1 and cohort_1" & estimate_name == "count") |>
               dplyr::pull(estimate_value))
  )


  overlap2 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = FALSE,
                                    timing = c("min", "q25",
                                               "median","q75",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap2))

  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() !=
      overlap2 |> dplyr::filter(group_level == "cohort_1 and cohort_1" & variable_name == "number records") |>
               dplyr::pull(estimate_value)
  )
  expect_true(
    cdm$cohort |>
      dplyr::filter(cohort_definition_id == 1) |> dplyr::distinct(subject_id) |>
      dplyr::tally() |> dplyr::pull() ==
      overlap2 |> dplyr::filter(group_level == "cohort_1 and cohort_1" & variable_name == "number subjects") |>
      dplyr::pull(estimate_value)
  )

  overlap3 <- summariseCohortTiming(cdm$cohort,
                                    restrictToFirstEntry = TRUE,
                                    timing = c("min",
                                               "max"))
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap3))

  expect_true(all(overlap3$estimate_name |> unique() %in%
                    c("min","max","count")))

  CDMConnector::cdm_disconnect(cdm)

})
