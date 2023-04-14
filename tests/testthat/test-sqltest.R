
test_that("test methods against sql test server", {
  skip_if(Sys.getenv("USER") == "")

  Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = jdbcDriversFolder)
  Sys.setenv(SERVER = darwinDbDatabaseServer)
  Sys.setenv(DATABASE = darwinDbDatabase)
  Sys.setenv(USER = darwinDbUser)
  Sys.setenv(PW = darwinDbPassword)
  Sys.setenv(PORT = darwinDbDatabasePort)
  Sys.setenv(DRIVER = darwinDbDriver)
  Sys.setenv(DBNAME = darwinDbName)

  server   <- Sys.getenv("SERVER")
  database <- Sys.getenv("DATABASE")
  user     <- Sys.getenv("USER")
  password <- Sys.getenv("PW")
  port     <- Sys.getenv("PORT")
  driver   <- Sys.getenv("DRIVER")
  dbName   <- Sys.getenv("DBNAME")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = driver,
                       Server   = server,
                       Database = database,
                       UID      = user,
                       PWD      = password,
                       Port     = port)




  cdm <- CDMConnector::cdm_from_con(db,
                                    cdm_schema = "cdm_synthea_1M",
                                    write_schema = "mdu",
                                    cohort_tables = c("cohort","cohort1"))


  # cohortToAdd <-
  #   cdm$condition_occurrence %>% select(
  #     "subject_id" = "person_id",
  #     "cohort_start_date" = "condition_start_date",
  #     "cohort_end_date" = "condition_end_date",
  #     "cohort_definition_id" = "condition_occurrence_id"
  #   ) %>%
  #   head(., 10000)
  #
  # cohortToAdd %>% computeQuery("cohort",
  #                              schema = "mdu",
  #                              temporary = FALSE,
  #                              overwrite = TRUE)



  #add age and add sex
  cdm$condition_occurrence <- cdm$condition_occurrence  %>%
    addAge(cdm = cdm, indexDate = "condition_start_date")  %>%
    addSex(cdm)

  expect_true(all(c("sex", "age") %in% names(cdm$condition_occurrence)))

  cdm$condition_occurrence %>%  addDemographics(
    cdm = cdm,
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


  #add demographics
  expect_true(all(c("sex", "age") %in% names(cdm$condition_occurrence)))


  #add cohort intersect

 cohort <- cdm$cohort %>% addIntersect(cdm = cdm,
                               tableName = "cohort1",
                               window = list(c(-Inf, 0)), value = "date")

 expect_true(all(c("date_NA_mInf_to_0") %in% names(cohort)))

 # add cohort occurrences

 cohort <- cdm$cohort %>%
   countOccurrences(cdm = cdm, tableName = "cohort1") %>%
   dplyr::arrange(subject_id, cohort_start_date)

 expect_true(all(c("NA_0_to_Inf") %in% names(cohort)))


 # count occurrences

cohort <-  cdm$cohort %>%
   countOccurrences(cdm = cdm, tableName = "cohort1") %>%
   dplyr::arrange(subject_id, cohort_start_date)

expect_true(all(c("NA_0_to_Inf") %in% names(cohort)))

# countflag
cohort <- cdm$cohort %>%
  flagPresence(cdm = cdm,tableName = "cohort1") %>%
  dplyr::arrange(subject_id, cohort_start_date)

expect_true(all(c("NA_0_to_Inf") %in% names(cohort)))



  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
