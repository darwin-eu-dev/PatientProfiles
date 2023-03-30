test_that("working examples", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  result <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$all_0_to_Inf == as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))


  result_1 <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "number") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "time") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "binary") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$all_0_to_Inf ==  c(4, 4, 3, 3, 1)))
  expect_true(all(result_1$all_0_to_Inf_1 == c(14, 0, 5, 23, 43)))
  expect_true(all(result_1$all_0_to_Inf_2 == c(1, 1, 1, 1, 1)))

  result_2 <-
    cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "date", order = "last") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "number", order = "last") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "time", order = "last") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", value = "binary", order = "last") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_2$all_0_to_Inf == as.Date(
    c(
      "2020-02-16",
      "2020-02-16",
      "2020-02-16",
      "2020-03-15",
      "2020-03-15"
    )
  )))
  expect_true(all(result_2$all_0_to_Inf_2 == c(46, 32, 27, 74, 43)))
  expect_true(all(result_2$all_0_to_Inf_1 == c(4, 4, 3, 3, 1)))
  expect_true(all(result_2$all_0_to_Inf_3 == c(1, 1, 1, 1, 1)))

  result_3 <-
    cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
                                       cohortTableName = "cohort2",
                                       window = list(c(-Inf, 0)), value = "date") %>%
    addCohortIntersect(cdm = cdm,
                       cohortTableName = "cohort2",
                       window = list(c(-Inf, 0)), value = "time") %>%
    addCohortIntersect(cdm = cdm,
                       cohortTableName = "cohort2",
                       window = list(c(-Inf, 0)), value = "number") %>%
    addCohortIntersect(cdm = cdm,
                       cohortTableName = "cohort2",
                       window = list(c(-Inf, 0)), value = "binary") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_3$all_mInf_to_0 %in% as.Date(
    c(NA, "2020-01-15", "2020-01-15", NA, "2020-01-24")
  )))
  expect_true(all(result_3$all_mInf_to_0_1 %in% c(NA, 0,-5, NA,-8)))
  expect_true(all(result_3$all_mInf_to_0_2 == c(0, 1, 1, 0, 2)))
  expect_true(all(result_3$all_mInf_to_0_3 == c(0, 1, 1, 0, 1)))

 result_4 <- cdm$cohort1 %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "date") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "time") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "number") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "binary") %>%
   dplyr::arrange(subject_id, cohort_start_date) %>%
   dplyr::collect()

 expect_true(all(result_4$all_m30_to_30 == as.Date(
   c(
     "2020-01-15",
     "2020-01-15",
     "2020-01-15",
     "2020-01-24",
     "2020-01-24"
   )
 )))
 expect_true(all(result_4$all_m30_to_30_1 == c(14, 0, -5, 23, -8)))
 expect_true(all(result_4$all_m30_to_30_2 == c(3, 3, 4, 2, 2)))
 expect_true(all(result_4$all_m30_to_30_3 == c(1, 1, 1, 1, 1)))

 result_5 <- cdm$cohort1 %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "date", order = "last") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "time", order = "last") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "number", order = "last") %>%
   addCohortIntersect(cdm = cdm,cohortTableName = "cohort2",window = list(c(-30, 30)), value = "binary", order = "last") %>%
   dplyr::arrange(subject_id, cohort_start_date) %>%
   dplyr::collect()

 expect_true(all(result_5$all_m30_to_30_2 == c(3, 3, 4, 2, 2)))
 expect_true(all(result_5$all_m30_to_30_3 == c(1, 1, 1, 1, 1)))
 expect_true(all(result_5$all_m30_to_30_1 == c(25, 11, 27, 28, -3)))
 expect_true(all(result_5$all_m30_to_30 == as.Date(
   c(
     "2020-01-26",
     "2020-01-26",
     "2020-02-16",
     "2020-01-29",
     "2020-01-29"
   )
 )))

 DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("working examples with cohort_end_date", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-20",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  result <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date",
                       indexDate = "cohort_end_date") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$all_0_to_Inf == as.Date(c("2020-01-25", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("working examples with multiple cohort Ids", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", cohortId = 1, value = "date") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(compareNA(result$cohort1_0_to_Inf, as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", NA, NA)))))

  result_1 <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", cohortId = 2, value = "number") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$cohort2_0_to_Inf == c(1,1,1,1,0)))


  result_2 <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", cohortId = c(1,3), value = "number") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", cohortId = c(1,3), value = "time") %>%
    addCohortIntersect(cdm = cdm, cohortTableName = "cohort2", cohortId = c(1,3),  value = "binary") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_2$cohort1_0_to_Inf ==  c(2, 2, 1, 0, 0)))
  expect_true(all(compareNA(result_2$cohort1_0_to_Inf_1,c(14, 0, 5, NA, NA))))
  expect_true(all(result_2$cohort1_0_to_Inf_2 == c(1, 1, 1, 0, 0)))
  expect_true(all(result_2$cohort3_0_to_Inf ==  c(1, 1, 1, 2, 1)))
  expect_true(all(result_2$cohort3_0_to_Inf_1 == c(46, 32, 27, 23, 43)))
  expect_true(all(result_2$cohort3_0_to_Inf_2 == c(1, 1, 1, 1, 1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("working examples calculating as incidence target cohort", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2021-01-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1),
    subject_id = c(1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-02-15"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date", nameStyle = "test_{cohortName}_{window}") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_1 <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date", targetEndDate = NULL) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$test_all_0_to_Inf == as.Date("2020-01-01")))
  expect_true(!("all_0_to_Inf" %in% colnames(result_1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("working examples with more than one window", {

  #functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  compareNA <- function(v1,v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date") %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date", window = list(c(-Inf,0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result_1 <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date", window = list(c(0,Inf),c(-Inf,0))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result$all_0_to_Inf == result_1$all_0_to_Inf))
  expect_true(all(compareNA(result$all_mInf_to_0, result_1$all_mInf_to_0)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("working examples for cohortName from cohortSet", {

  skip_if_not_installed("duckdb")
  skip_if_not(CDMConnector::eunomia_is_available())
  skip_if_not_installed("CirceR")

  # Connect to Eunomia cdm
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  write_schema <- "main"
  cdm_schema <- "main"

  cdm <- CDMConnector::cdm_from_con(con,
                      cdm_schema = cdm_schema,
                      cdm_tables = c(CDMConnector::tbl_group("default")),
                      write_schema = write_schema)

  # Read jsons form inst
  cohortSet <- CDMConnector::readCohortSet(path = system.file("cohorts",
                                                              package = "PatientProfiles"))
  expect_equal(nrow(cohortSet), 3)

  # Generate small cohort sets and check
  cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "testpp", computeAttrition = FALSE)

  expect_s3_class(CDMConnector::cohortSet(cdm$testpp), "tbl_dbi")

  # Try functionality of addCohortIntersect
  result <- cdm$testpp %>% addCohortIntersect(cdm, cohortTableName = "testpp", value = "binary") %>%
    dplyr::collect()

  expect_true(result %>% dplyr::tally() %>% dplyr::pull() == cdm$testpp %>% dplyr::tally() %>% dplyr::pull())
  expect_true("GIBleed_male_0_to_Inf" %in% colnames(result))

  DBI::dbDisconnect(con, shutdown = TRUE)

})

test_that("check input length and type for each of the arguments", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-01",
        "2020-01-15",
        "2020-01-20",
        "2020-01-01",
        "2020-02-01"
      )
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
    cohort_end_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2020-01-26",
        "2020-01-29",
        "2020-03-15",
        "2020-01-24",
        "2020-02-16"
      )
    ),
  )

  cdm <- mockPatientProfiles(cohort1=cohort1, cohort2=cohort2)

  expect_error(addCohortIntersect("cdm$cohort1", cdm))

  expect_error(addCohortIntersect(cdm$cohort1, "cdm"))

  expect_error(addCohortIntersect(cdm$cohort1, "cdm", cohortTableName = "drug"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "end_date"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", cohortId = "3"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", window = c(1,3)))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", indexDate = 3))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", targetStartDate = "no"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", targetEndDate = NA))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, cohortTableName = "cohort2", value = "binary", nameStyle = "test_{nowindow}_{cohortName}"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})
