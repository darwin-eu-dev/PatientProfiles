test_that("test methods against sql test server", {
  testthat::skip_on_ci()

  server <- Sys.getenv("SERVER")
  database <- Sys.getenv("DATABASE")
  user <- Sys.getenv("USER")
  password <- Sys.getenv("PW")
  port <- Sys.getenv("PORT")
  driver <- Sys.getenv("DRIVER")
  dbName <- Sys.getenv("DBNAME")

  db <- DBI::dbConnect(odbc::odbc(),
    Driver   = driver,
    Server   = server,
    Database = database,
    UID      = user,
    PWD      = password,
    Port     = port
  )




  cdm <- CDMConnector::cdm_from_con(db,
    cdm_schema = "cdm_synthea_1M",
    write_schema = "mdu",
    cohort_tables = c("cohort", "cohort1")
  )

  # add age and add sex
  cdm$condition_occurrence <- cdm$condition_occurrence %>%
    addAge(indexDate = "condition_start_date") %>%
    addSex()

  expect_true(all(c("sex", "age") %in% colnames(cdm$condition_occurrence)))

  cdm$condition_occurrence %>% addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup = NULL,
    sex = TRUE,
    sexName = "sex",
    priorHistory = FALSE,
    priorHistoryName = "prior_history",
    futureObservation = FALSE,
    indexDate = "condition_start_date"
  )


  # add demographics
  expect_true(all(c("sex", "age") %in% colnames(cdm$condition_occurrence)))


  # add cohort intersect

  cohort <- cdm$cohort %>% addIntersect(
    tableName = "cohort1",
    window = list(c(-Inf, 0)), value = "date"
  )

  expect_true(all(c("all_0_to_inf") %in% colnames(cohort)))

  # add cohort occurrences

  cohort <- cdm$cohort %>%
    addCohortIntersectCount(targetCohortTable = "cohort1") %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(c("all_0_to_inf") %in% colnames(cohort)))


  # countflag
  cohort <- cdm$cohort %>%
    addCohortIntersectFlag(targetCohortTable = "cohort1") %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(c("all_0_to_inf") %in% colnames(cohort)))


  # time to cohort
  cohort <- cdm$cohort %>%
    addCohortIntersectTime(targetCohortTable = "cohort1") %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(c("all_0_to_inf") %in% colnames(cohort)))

  # dateOfCohort

  cohort <- cdm$cohort %>%
    addCohortIntersectDate(targetCohortTable = "cohort1") %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(c("all_0_to_inf") %in% colnames(cohort)))
})
