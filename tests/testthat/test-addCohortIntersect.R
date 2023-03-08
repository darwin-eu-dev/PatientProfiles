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

  cdm <- mockCohortProfiles(cohort1=cohort1, cohort2=cohort2)

  result <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm,cohortTableName = "cohort2", value = "date") %>% dplyr::collect()

  expect_true(all(result$date_cohort2_1 == as.Date(c("2020-01-15", "2020-01-15", "2020-01-25", "2020-01-24", "2020-03-15"))))


  result_1 <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm,cohortTableName = "cohort2") %>% dplyr::collect()

  expect_true(all(result_1$date_cohort2_1 == as.Date(
    c(
      "2020-01-15",
      "2020-01-15",
      "2020-01-25",
      "2020-01-24",
      "2020-03-15"
    )
  )))
  expect_true(all(result_1$"time_cohort2_(0,NA)_first_1" == c(14, 0, 5, 23, 43)))
  expect_true(all(result_1$"number_cohort2_(0,NA)_1" == c(4, 4, 3, 3, 1)))
  expect_true(all(result_1$"binary_cohort2_(0,NA)_1" == c(1, 1, 1, 1, 1)))

  result_2 <-
    cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
                                       cohortTableName = "cohort2",
                                       order = "last") %>% dplyr::collect()
  expect_true(all(result_2$"date_cohort2_(0,NA)_last_1" == as.Date(
    c(
      "2020-02-16",
      "2020-02-16",
      "2020-02-16",
      "2020-03-15",
      "2020-03-15"
    )
  )))
  expect_true(all(result_2$"time_cohort2_(0,NA)_last_1" == c(46, 32, 27, 74, 43)))
  expect_true(all(result_2$"number_cohort2_(0,NA)_1" == c(4, 4, 3, 3, 1)))
  expect_true(all(result_2$"binary_cohort2_(0,NA)_1" == c(1, 1, 1, 1, 1)))

  result_3 <-
    cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
                                       cohortTableName = "cohort2",
                                       window = c(NA, 0)) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_3$"date_cohort2_(NA,0)_first_1" %in% as.Date(
    c(NA, "2020-01-15", "2020-01-15", NA, "2020-01-24")
  )))
  expect_true(all(result_3$"time_cohort2_(NA,0)_first_1" %in% c(NA, 0,-5, NA,-8)))
  expect_true(all(result_3$"number_cohort2_(NA,0)_1" == c(0, 1, 1, 0, 2)))
  expect_true(all(result_3$"binary_cohort2_(NA,0)_1" == c(0, 1, 1, 0, 1)))

 result_4 <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
                                                cohortTableName = "cohort2",
                                                window = c(-30, 30)) %>%
   dplyr::arrange(subject_id, cohort_start_date) %>%
   dplyr::collect()

 expect_true(all(result_4$"date_cohort2_(-30,30)_first_1" == as.Date(
   c(
     "2020-01-15",
     "2020-01-15",
     "2020-01-15",
     "2020-01-24",
     "2020-01-24"
   )
 )))
 expect_true(all(result_4$"time_cohort2_(-30,30)_first_1" == c(14, 0, -5, 23, -8)))
 expect_true(all(result_4$"number_cohort2_(-30,30)_1" == c(3, 3, 4, 2, 2)))
 expect_true(all(result_4$"binary_cohort2_(-30,30)_1" == c(1, 1, 1, 1, 1)))

 result_5 <- cdm$cohort1 %>% addCohortIntersect(cdm = cdm,
                                                cohortTableName = "cohort2",
                                                window = c(-30, 30),
                                                order = "last") %>%
   dplyr::arrange(subject_id, cohort_start_date) %>%
   dplyr::collect()

 expect_true(all(result_5$"number_cohort2_(-30,30)_1" == c(3, 3, 4, 2, 2)))
 expect_true(all(result_5$"binary_cohort2_(-30,30)_1" == c(1, 1, 1, 1, 1)))
 expect_true(all(result_5$"time_cohort2_(-30,30)_last_1" == c(25, 11, 27, 28, -3)))
 expect_true(all(result_5$"date_cohort2_(-30,30)_last_1" == as.Date(
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

  cdm <- mockCohortProfiles(cohort1=cohort1, cohort2=cohort2)

  expect_error(addCohortIntersect("cdm$cohort1", cdm))

  expect_error(addCohortIntersect(cdm$cohort1, "cdm"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "end_date"))

  expect_error(addCohortIntersect(cdm$cohort1, cdm, value = "binary", order = "lol"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})
