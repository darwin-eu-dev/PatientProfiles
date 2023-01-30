test_that("check addEvent using drug_exposure table as example", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  cdm <- mockCohortProfiles(seed = 100, cohort1 = cohort1)

  #check eventDate and eventAt input
  expect_error(
    addEvent(
      cdm$cohort1,
      cdm,
      "drug_exposure",
      window = c(0, 10000),
      eventDate = "cohort_start_date"
    )
  )

  expect_error(addEvent(cdm$cohort1, cdm, "drug_exposure", window = c(0, 10000)))

  # addEvent to check correct output for order
  test_1 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    order = "first"
  )

  test_2 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    order = "last"
  )

  test_3 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    order = "all"
  )

  #addEvent to test window
  test_4 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 1000),
    eventDate = "drug_exposure_start_date",
    order = "all"
  )

  #test correct output for test_1
  expect_true("event" %in% colnames(test_1))
  expect_true(
    test_1 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2010-03-30"
  )
  expect_true(is.na(
    test_1 %>% dplyr::filter(subject_id == 2) %>% dplyr::select(event) %>% dplyr::pull()
  ))
  expect_true(is.na(
    test_1 %>% dplyr::filter(subject_id == 3) %>% dplyr::select(event) %>% dplyr::pull()
  ))

  #test correct output for test_2
  expect_true("event" %in% colnames(test_2))
  expect_true(
    test_2 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2018-07-16"
  )
  expect_true(is.na(
    test_2 %>% dplyr::filter(subject_id == 2) %>% dplyr::select(event) %>% dplyr::pull()
  ))
  expect_true(is.na(
    test_2 %>% dplyr::filter(subject_id == 3) %>% dplyr::select(event) %>% dplyr::pull()
  ))

  #test correct output for test_3
  expect_false("event" %in% colnames(test_3))
  expect_true(all(
    c(
      "event_1",
      "event_2",
      "event_3",
      "event_4",
      "event_5",
      "event_6"
    ) %in% colnames(test_3)
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_1) %>% dplyr::pull() == c("2010-03-30")
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_2) %>% dplyr::pull() == c("2013-11-14")
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_3) %>% dplyr::pull() == c("2014-06-13")
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_4) %>% dplyr::pull() == c("2014-06-22")
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_5) %>% dplyr::pull() == c("2014-11-03")
  ))
  expect_true(any(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_6) %>% dplyr::pull() == c("2018-07-16")
  ))

  #test correct output for test_4
  expect_false("event" %in% colnames(test_4))
  expect_true(all(c("event_1") %in% colnames(test_3)))
  expect_true(any(
    test_4 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event_1) %>% dplyr::pull() == c("2010-03-30")
  ))
  expect_true(is.na(
    test_4 %>% dplyr::filter(subject_id == 2) %>% dplyr::select(event_1) %>% dplyr::pull()
  ))
  expect_true(is.na(
    test_4 %>% dplyr::filter(subject_id == 3) %>% dplyr::select(event_1) %>% dplyr::pull()
  ))




  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("check addEvent filter input", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01"),
      as.Date("2013-01-01")
    )
  )

  cdm <- mockCohortProfiles(seed = 100, cohort1 = cohort1)


  # addEvent to check correct output for order
  test_1 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    filter = list(drug_concept_id = 1)
  )

  test_2 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    filter = list(drug_concept_id = 2)
  )

  test_3 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    filter = list(drug_concept_id = c(1, 2))
  )

  test_4 <- addEvent(
    cdm$cohort1,
    cdm,
    "drug_exposure",
    window = c(0, 10000),
    eventDate = "drug_exposure_start_date",
    filter = list(drug_concept_id = c(1, 2)),
    order = "last"
  )

  #test correct output for test_1, test_2,test_3
  expect_true("event" %in% colnames(test_1))
  expect_true(
    test_1 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2013-11-14"
  )
  expect_true(
    test_2 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2018-07-16"
  )
  expect_true(
    test_3 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2013-11-14"
  )
  expect_true(
    test_4 %>% dplyr::filter(subject_id == 1) %>% dplyr::select(event) %>% dplyr::pull() == "2018-07-16"
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
