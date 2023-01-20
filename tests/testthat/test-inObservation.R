test_that("inObservation", {
 cdm <- mockDrugUtilisation(seed = 11, patient_size = 10, earliest_observation_start_date = as.Date("2010-01-01"), latest_observation_start_date = as.Date("2022-01-01"))

 result1 <- inObservation(cdm$cohort1,cdm)
 result2 <- inObservation(cdm$cohort2,cdm)

 expect_true("in_observation" %in% colnames(result1))
 expect_true(all(result1 %>% dplyr::select(in_observation) %>% dplyr::pull() == c(TRUE,TRUE,FALSE,TRUE)))
 expect_true("in_observation" %in% colnames(result1))
 expect_true(all(result2 %>% dplyr::select(in_observation) %>% dplyr::pull() == c(TRUE,FALSE,TRUE,TRUE,TRUE)))

 expect_error(inObservation(2,cdm))
 expect_error(inObservation(cdm$cohort2,"cdm"))
 expect_error(inObservation(cdm$person,cdm))
 expect_error(inObservation(cdm$cohort1,cdm,observationAt = 3))
 expect_error(inObservation(cdm$cohort1,cdm,observationAt = "2002-01-02"))
 expect_error(inObservation(cdm$cohort2,cdm,name = 3))
 expect_error(inObservation(cdm$cohort2,cdm,compute = 2))

 DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
