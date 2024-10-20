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

  expect_no_error(
    cdm$cohort1a <- cdm$cohort1 %>%
    addConceptIntersectFlag(
      conceptSet = list("not_in_concept_table" = 99L),
      nameStyle = "new_col"
    )
  )
 expect_true(all(cdm$cohort1a |>
    dplyr::pull("new_col") == 0L))

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
    cohort_definition_id = 1L,
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

test_that("records out of observation", {

  cdm <- mockPatientProfiles(con = connection(),
                             writeSchema = writeSchema())
  cdm <- omopgenerics::insertTable(
    cdm, "observation_period",
    table = data.frame(observation_period_id = 1L,
                       person_id = 1L,
                       observation_period_start_date = as.Date("2000-01-01"),
                       observation_period_end_date  = as.Date("2010-01-01"),
                       period_type_concept_id = 1L))
  cdm <- omopgenerics::insertTable(
    cdm, "my_cohort",
    table = data.frame(cohort_definition_id = 1L,
                       subject_id = 1L,
                       cohort_start_date = as.Date("2000-01-01"),
                       cohort_end_date = as.Date("2010-01-01")))
  # add a concept to put out of observation
  cdm <- omopgenerics::insertTable(cdm, "concept",
                            table = data.frame(concept_id = 99L,
                                               concept_name = "concept",
                                               domain_id = "condition",
                                               vocabulary_id = "test",
                                               concept_class_id = 1L,
                                               concept_code = 99L,
                                               valid_start_date = as.Date("1900-01-01"),
                                               valid_end_date = as.Date("2099-01-01")))

  cdm <- omopgenerics::insertTable(cdm, "condition_occurrence",
                                   table = data.frame(condition_occurrence_id = c(1L, 2L),
                                                      person_id = c(1L, 1L),
                                                      condition_concept_id = c(99L, 99L),
                                                      condition_start_date = c(as.Date("1990-01-01"),
                                                                               as.Date("1991-01-01")),
                                                      condition_end_date= c(as.Date("1990-01-01"),
                                                                            as.Date("1991-01-01")),
                                                      condition_type_concept_id = c(1L, 1L))
  )


  # default - record out of observation will be excluded
  cdm$my_cohort <- cdm$my_cohort |>
   addConceptIntersectFlag(conceptSet = list(a = 99L),
                           window = list(c(-Inf, Inf)),
                           inObservation = TRUE,
                           nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
    dplyr::pull("intersect") == 0)

  # include records out of observation
  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectFlag(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = FALSE,
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") == 1)
  # not if outside of window
  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectFlag(conceptSet = list(a = 99L),
                            window = list(c(0, Inf)),
                            inObservation = FALSE,
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") == 0)

expect_error(cdm$my_cohort |>
    addConceptIntersectFlag(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = "not_logical", # should cause error
                            nameStyle = "intersect"))

  # count
  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectCount(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = FALSE,
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") == 2)

  # date
  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectDate(conceptSet = list(a = 99L),
                             window = list(c(-Inf, Inf)),
                             inObservation = FALSE,
                              order = "first",
                             nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") == "1990-01-01")

  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectDate(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = FALSE,
                            order = "last",
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") == "1991-01-01")

   # days
  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectDays(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = FALSE,
                            order = "last",
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") ==
                as.integer(difftime(as.Date("1991-01-01"),
                         as.Date("2000-01-01"))))

  cdm$my_cohort <- cdm$my_cohort |>
    addConceptIntersectDays(conceptSet = list(a = 99L),
                            window = list(c(-Inf, Inf)),
                            inObservation = FALSE,
                            order = "first",
                            nameStyle = "intersect")
  expect_true(cdm$my_cohort |>
                dplyr::pull("intersect") ==
                as.integer(difftime(as.Date("1990-01-01"),
                                    as.Date("2000-01-01"))))


})
