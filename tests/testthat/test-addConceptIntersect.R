test_that("addConceptIntersect", {
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdmFromCon(
    con = con, cdmSchema = "main", writeSchema = "main"
  )
  cdm <- CDMConnector::copyCdmTo(
    con = connection(), cdm = cdm, schema = writeSchema()
  )
  DBI::dbDisconnect(conn = con, shutdown = TRUE)

  # create a cohort
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm, conceptSet = list("sinusitis" = c(4294548L, 40481087L, 257012L)),
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

  mockDisconnect(cdm = cdm)
})

test_that("unsupported domain name", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
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
      conceptSet = list("random" = 1125315L)
    ) %>%
    dplyr::collect())

  expect_true(
    "random_0_to_inf" %in%
      (result |>
        colnames())
  )

  mockDisconnect(cdm = cdm)
})

test_that("NA domain name", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
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
      conceptSet = list("random2" = 1125315L)
    ) %>%
    dplyr::collect())

  expect_true(
    "random2_0_to_inf" %in%
      (result |>
        colnames())
  )
  mockDisconnect(cdm = cdm)
})

test_that("domain name not in cdm", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  concept <- dplyr::tibble(
    concept_id = c(1125315L),
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
      conceptSet = list("random3" = 1125315L)
    ) %>%
    dplyr::collect())

  expect_true(
    "random3_0_to_inf" %in%
      (result |>
        colnames())
  )

  mockDisconnect(cdm = cdm)
})

test_that("missing event end date", {
  skip_on_cran()
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
  }
  if (!CDMConnector::eunomia_is_available()) {
    invisible(utils::capture.output(CDMConnector::downloadEunomiaData(pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"))))
  }
  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )

  cohort <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = 273L,
    cohort_start_date = as.Date("2012-10-10"),
    cohort_end_date = as.Date("2013-10-10")
  )

  DBI::dbWriteTable(con, "cohort", cohort)
  cdm <- CDMConnector::cdm_from_con(con,
    cdm_schema = "main", write_schema = "main",
    cohort_tables = "cohort"
  )

  cdm <- cdm %>%
    CDMConnector::cdm_subset(person_id = 273L)


  expect_true(cdm$cohort %>%
    PatientProfiles::addConceptIntersectFlag(
      conceptSet = list(a = 192671L),
      window = c(-Inf, 0)
    ) |>
    dplyr::pull("a_minf_to_0") == 1)


  mockDisconnect(cdm)
})
