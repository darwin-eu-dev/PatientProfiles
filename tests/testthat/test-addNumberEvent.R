test_that("event drug exposure",{
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )
  cdm <- mockCohortProfiles(seed = 207, cohort1 = cohort1)
  events <- addNumberEvent(cdm$cohort1, cdm, "drug_exposure", window = c(0,700), eventDate = "drug_exposure_start_date")
  expect_true("number_event" %in% colnames(events))
  expect_true(sum(events %>% dplyr::select(number_event) %>% dplyr::pull()) == 1)
  events2 <- addNumberEvent(cdm$cohort1, cdm, "drug_exposure", window = c(0,500), eventDate = "drug_exposure_start_date")
  expect_true("number_event" %in% colnames(events2))
  expect_true(sum(events2 %>% dplyr::select(number_event) %>% dplyr::pull()) == 0)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("event condition",{
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-10"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )
  cdm <- mockCohortProfiles(seed = 3, cohort1 = cohort1)
  events <- addNumberEvent(cdm$cohort1, cdm, "condition_occurrence", window = c(0,100), eventAt = "cohort_end_date",eventDate = "condition_end_date")
  expect_true("number_event" %in% colnames(events))
  expect_true(sum(events %>% dplyr::select(number_event) %>% dplyr::pull()) == 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("drug exposure, filters",{
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )
  cdm <- mockCohortProfiles(seed = 207, cohort1 = cohort1)
  events <- addNumberEvent(cdm$cohort1, cdm, "drug_exposure", filter = list(drug_concept_id = 2), window = c(0,700), eventDate = "drug_exposure_start_date")
  expect_true("number_event" %in% colnames(events))
  expect_true(sum(events %>% dplyr::select(number_event) %>% dplyr::pull()) == 0)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors",{
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-01"), as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2013-01-01")
    )
  )
  cdm <- mockCohortProfiles(seed = 207, cohort1 = cohort1)
  expect_error(addNumberEvent(cdm$cohort1, "cdm", "drug_exposure", filter = c(drug_concept_id = 2), window = c(0,700), eventDate = "drug_exposure_start_date"))
  expect_error(addNumberEvent(cdm, cdm, "drug_exposure", filter = c(drug_concept_id = 2), window = c(0,700), eventDate = "drug_exposure_start_date"))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "cohort4", filter = c(drug_concept_id = 2), window = c(0,700), eventDate = "drug_exposure_start_date"))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", filter = c(drug_concept_id = 2), window = 20, eventDate = "drug_exposure_start_date"))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", window = c(0,10), eventDate = "cohort_start_date"))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", window = c(0,10), eventAt = 2))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", window = c(0,10), eventDate = "drug_exposure_start_date", filter = list("filter")))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", filter = c(drug_concept_id = 2), window = c(0,10), eventDate = "drug_exposure_start_date", name = 2))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", filter = c(drug_concept_id = 2), window = c(0,10), eventDate = "drug_exposure_start_date", name = c("name1","name2")))
  expect_error(addNumberEvent(cdm$cohort2, cdm, "drug_exposure", filter = c(drug_concept_id = 2), window = c(0,10), eventDate = "drug_exposure_start_date", compute = 2))
  cdm$condition_occurrence <- cdm$condition_occurrence %>% dplyr::mutate(subject_id = person_id)
  expect_error(addNumberEvent(cdm$cohort1, cdm, "condition_occurrence", window = c(0,100), eventDate = "condition_end_date"))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})







