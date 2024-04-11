test_that("addConceptIntersect", {
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

  # create a cohort
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm, conceptSet = list("sinusitis" = c(4294548, 40481087, 257012)),
    name = "my_cohort"
  )

  codelist <- list(
    "statin" = cdm$concept |>
      dplyr::filter(grepl("statin", concept_name, ignore.case = T)) |>
      dplyr::pull("concept_id"),
    "serum_measurement" = cdm$concept |>
      dplyr::filter(grepl("serum", concept_name, ignore.case = T)) |>
      dplyr::pull("concept_id"),
    "allergy" = cdm$concept |>
      dplyr::filter(grepl("allergy", concept_name, ignore.case = T)) |>
      dplyr::pull("concept_id"),
    "bypass" = cdm$concept |>
      dplyr::filter(grepl("bypass", concept_name, ignore.case = T)) |>
      dplyr::pull("concept_id"),
    "laceration" = cdm$concept |>
      dplyr::filter(grepl("laceration", concept_name, ignore.case = T)) |>
      dplyr::pull("concept_id")
  )

  expect_warning(cdm$my_cohort |>
                   addConceptIntersect(conceptSet = codelist))

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectCount(conceptSet = codelist)
  )

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectFlag(conceptSet = codelist)
  )

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectDate(conceptSet = codelist)
  )

  expect_no_error(
    cdm$my_cohort |>
      summariseCharacteristics(conceptIntersect = list("my concets" = list(
        conceptSet = codelist, value = "count", window = list(c(0, Inf), c(0, 90))
      )))
  )

  expect_no_error(
    cdm$my_cohort |>
      summariseCharacteristics(conceptIntersect = list("my concets" = list(
        conceptSet = codelist, value = "flag", window = c(0, Inf)
      )))
  )

  expect_no_error(
    cdm$my_cohort |>
      summariseConceptIntersect(conceptIntersect = list("my concets" = list(
        conceptSet = codelist, value = "days", window = c(0, Inf)
      )))
  )

  expect_no_error(
    cdm$my_cohort |>
      summariseConceptIntersect(conceptIntersect = list("my concets" = list(
        conceptSet = codelist, value = "date", window = c(0, Inf)
      )))
  )


  # test input
  expect_error(
    cdm$my_cohort |>
      addConceptIntersectCount(
        conceptSet = codelist, targetStartDate = "cohort_start_date"
      )
  )
  expect_error(
    cdm$my_cohort |>
      addConceptIntersectCount(
        conceptSet = codelist, targetEndDate = "cohort_start_date"
      )
  )

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectDays(
        conceptSet = codelist, targetDate = "event_end_date"
      )
  )

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectDate(
        conceptSet = codelist, targetDate = "event_end_date"
      )
  )

  expect_no_error(
    cdm$my_cohort |>
      addConceptIntersectCount(
        conceptSet = codelist, targetStartDate = "event_end_date"
      )
  )

  expect_error(
    cdm$my_cohort |>
      addConceptIntersectCount(
        conceptSet = codelist, targetStartDate = NULL
      )
  )

  CDMConnector::cdmDisconnect(cdm = cdm)
})
