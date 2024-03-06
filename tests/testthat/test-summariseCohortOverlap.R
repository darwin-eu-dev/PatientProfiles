test_that("expected output", {

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 2, tableName = "table1") |>
    omock::mockCohort(numberCohorts = 5, tableName = "table2")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm_local,
                                   schema = "main")

  overlap1 <- summariseCohortOverlap(cdm$table1)
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap1))
  expect_equal(overlap1$group_name |> unique(),
               "cohort_name_reference and cohort_name_comparator")
  expect_true(nrow(overlap1) == 8)
  expect_equal(cdm$table1 |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::distinct(subject_id) |>
                 dplyr::tally() |> dplyr::pull(),
               as.numeric(overlap1$estimate_value[
                 overlap1$variable_name == "number subjects" &
                   overlap1$group_level == "cohort_1 and cohort_1"
               ])
  )
  expect_equal(cdm$table1 |>
                 dplyr::filter(cohort_definition_id == 1) |>
                 dplyr::tally() |> dplyr::pull(),
               as.numeric(overlap1$estimate_value[
                 overlap1$variable_name == "number records" &
                   overlap1$group_level == "cohort_1 and cohort_1"
               ])
  )

  overlap2 <- summariseCohortOverlap(cdm$table2)
  expect_true(nrow(overlap2) == 50)
  expect_equal(omopgenerics::resultColumns("summarised_result"),
               colnames(overlap2))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("tableCohortOverlap", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 5, tableName = "table")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm_local,
                                   schema = "main")

  overlap <- summariseCohortOverlap(cdm$table)

  gtResult1 <- tableCohortOverlap(overlap, cohortNameReference = "cohort_1")
  expect_true("gt_tbl" %in% class(gtResult1))
  expect_equal(gtResult1$`_data`$`Database name`,
               c("mock database", rep("", nrow(gtResult1$`_data`)-1)))
  expect_equal(unique(gtResult1$`_data`$`Cohort name reference`)[1], "cohort_1")
  expect_equal(unique(gtResult1$`_data`$`Cohort name comparator`),
               c("cohort_2", "", "cohort_3", "cohort_4", "cohort_5"))

  fxResult1 <- tableCohortOverlap(overlap,
                                  type = "flextable",
                                  minCellCount = 1000,
                                  variableName = "number records")
  expect_true("flextable" %in% class(fxResult1))
  expect_true(all(grepl("<1000", fxResult1$body$dataset$Overlap)))
  expect_false("number subjects" %in% fxResult1$body$dataset$`Variable name`)

  tibbleResult1 <-  tableCohortOverlap(overlap, type = "tibble")
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(tibbleResult1)))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("plotCohortOverlap", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(100) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(numberCohorts = 5, tableName = "table")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  cdm <- CDMConnector::copy_cdm_to(con = con,
                                   cdm = cdm_local,
                                   schema = "main")

  overlap <- summariseCohortOverlap(cdm$table)

  gg1 <- plotCohortOverlap(overlap, cohortLabels = "{cdm_name}; {cohort_name_reference}; {cohort_name_comparator}")
  expect_true("ggplot" %in% class(gg1))
  expect_true(gg1$data$total |> unique() == 100)
  expect_true(gg1$data |> dplyr::filter(variable_name == "number records") |> nrow() == 0)

  gg2 <- plotCohortOverlap(overlap, percentage = FALSE, subjects = FALSE, facetBy = "cdm_name")
  expect_true("ggplot" %in% class(gg2))
  expect_true(gg2$data |> dplyr::filter(variable_name == "number subjects") |> nrow() == 0)
  expect_true(any(gg2$data$total > 100))

  CDMConnector::cdm_disconnect(cdm)
})
