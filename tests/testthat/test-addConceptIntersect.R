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
      CohortCharacteristics::summariseCharacteristics(
        conceptIntersectCount = list("my concets" = list(
        conceptSet = codelist, window = list(c(0, Inf))
      )))
  )

  expect_no_error(
    cdm$my_cohort |>
      CohortCharacteristics::summariseCharacteristics(
        conceptIntersectFlag = list("my concets" = list(
          conceptSet = codelist, window = list(c(0, Inf))
        )))
  )

  expect_no_error(
    cdm$my_cohort |>
      CohortCharacteristics::summariseCharacteristics(
        conceptIntersectDays = list("my concets" = list(
        conceptSet = codelist, window = c(0, Inf)
      )))
  )

  expect_no_error(
    cdm$my_cohort |>
        CohortCharacteristics::summariseCharacteristics(
        conceptIntersectDate = list("my concets" = list(
        conceptSet = codelist, window = c(0, Inf)
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

test_that("unsupported domain name", {
  skip_on_cran()
  cdm <- mockPatientProfiles()
  concept <- dplyr::tibble(
    concept_id = c(1125315),
    domain_id = "random",
    concept_class_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01"),
    invalid_reason = NA_character_
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
  cdm <- CDMConnector::insertTable(cdm, "concept", concept)

  expect_no_warning(result <- cdm$cohort1 %>%
    addConceptIntersectFlag(
      conceptSet = list("random"=1125315)
    ) %>%
    dplyr::collect())

  expect_true(
    "random_0_to_inf" %in%
      (result |>
         colnames())
  )

  expect_warning(result <- cdm$cohort1 %>%
                      addConceptIntersect(
                        conceptSet = list("random"=1125315)
                      ) %>%
                      dplyr::collect())
  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("NA domain name", {
  skip_on_cran()
  cdm <- mockPatientProfiles()
  concept <- dplyr::tibble(
    concept_id = c(1125315),
    domain_id = NA_character_,
    concept_class_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01"),
    invalid_reason = NA_character_
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
  cdm <- CDMConnector::insertTable(cdm, "concept", concept)

  expect_no_warning(result <- cdm$cohort1 %>%
                      addConceptIntersectFlag(
                        conceptSet = list("random2"=1125315)
                      ) %>%
                      dplyr::collect())

  expect_true(
    "random2_0_to_inf" %in%
      (result |>
         colnames())
  )

  expect_warning(result <- cdm$cohort1 %>%
                   addConceptIntersect(
                     conceptSet = list("random2"=1125315)
                   ) %>%
                   dplyr::collect())
  CDMConnector::cdmDisconnect(cdm = cdm)
})

test_that("domain name not in cdm", {
  skip_on_cran()
  cdm <- mockPatientProfiles()
  concept <- dplyr::tibble(
    concept_id = c(1125315),
    domain_id = "device",
    concept_class_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01"),
    invalid_reason = NA_character_
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))
  cdm <- CDMConnector::insertTable(cdm, "concept", concept)

  expect_no_warning(result <- cdm$cohort1 %>%
                      addConceptIntersectFlag(
                        conceptSet = list("random3"=1125315)
                      ) %>%
                      dplyr::collect())

  expect_true(
    "random3_0_to_inf" %in%
      (result |>
         colnames())
  )

  expect_warning(result <- cdm$cohort1 %>%
                   addConceptIntersect(
                     conceptSet = list("random3"=1125315)
                   ) %>%
                   dplyr::collect())
  CDMConnector::cdmDisconnect(cdm = cdm)
})
