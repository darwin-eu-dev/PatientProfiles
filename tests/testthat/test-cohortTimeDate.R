# workaround to add cohort attributes to mock cohort table
addCohortCountAttr <- function(cohort) {
  cohort_count <- cohort %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::tally() %>%
    dplyr::collect()

  attr(cohort, "cohort_count") <- cohort_count
  attr(cohort, "cohort_set") <- cohort_count %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::mutate("cohort_name" = paste0(
      "cohort_",
      cohort_definition_id
    ))

  return(cohort)
}

test_that("output format - one outcome cohort", {
  # additional column should be added
  # with the name as specified

  cdm <- mockPatientProfiles()
  cdm$cohort1 <- addCohortCountAttr(cdm$cohort1)
  cdm$cohort2 <- addCohortCountAttr(cdm$cohort2)

  cdm$cohort1a <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      targetCohortId = 1,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1a) == 5)

  cdm$cohort1b <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      targetCohortId = 1,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1b) == 5)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("output format - multiple outcome cohorts", {
  # additional columns (one per outcome cohort) should be added
  # with the name as specified

  cdm <- mockPatientProfiles()
  cdm$cohort1 <- addCohortCountAttr(cdm$cohort1)
  cdm$cohort2 <- addCohortCountAttr(cdm$cohort2)

  # In 0 to Inf - 2 target cohorts have someone
  cdm$cohort1a <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1a) == 7)
  cdm$cohort1b <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1b) == 7)

  # In -Inf to Inf - 2 target cohorts have someone
  cdm$cohort1c <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      window = c(-Inf, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1c) == 7)
  cdm$cohort1d <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      window = c(-Inf, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1d) == 7)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("first vs last event - cohort table", {
  # depending on user choice, should get back either the
  # first or last outcome record

  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2010-03-01"),
      as.Date("2011-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2013-01-01")
    )
  )

  cohort2 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1", "1", "1", "2"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-15"),
      as.Date("2010-03-25"),
      as.Date("2013-01-03")
    ),
    cohort_end_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-15"),
      as.Date("2010-03-25"),
      as.Date("2013-01-03")
    )
  )

  cdm <- mockPatientProfiles(
    cohort1 = cohort1,
    cohort2 = cohort2
  )
  cdm$cohort1 <- addCohortCountAttr(cdm$cohort1)
  cdm$cohort2 <- addCohortCountAttr(cdm$cohort2)

  # first
  cdm$cohort1a <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )
  expect_true(cdm$cohort1a %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::pull(5) ==
    as.numeric(difftime(as.Date("2010-03-03"),
      as.Date("2010-03-01"),
      units = "days"
    )))
  expect_true(cdm$cohort1a %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::pull(5) ==
    as.numeric(difftime(as.Date("2013-01-03"),
      as.Date("2011-02-01"),
      units = "days"
    )))

  cdm$cohort1b <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )
  expect_true(cdm$cohort1b %>%
                dplyr::filter(subject_id == 1) %>%
                dplyr::pull(5) == as.Date("2010-03-03"))
  expect_true(cdm$cohort1b %>%
                dplyr::filter(subject_id == 2) %>%
                dplyr::pull(5) == as.Date("2013-01-03"))


  # last
  cdm$cohort1c <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "last"
    )
  expect_true(cdm$cohort1c %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::pull(5) ==
    as.numeric(difftime(as.Date("2010-03-25"),
      as.Date("2010-03-01"),
      units = "days"
    )))
  expect_true(cdm$cohort1c %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::pull(5) ==
    as.numeric(difftime(as.Date("2013-01-03"),
      as.Date("2011-02-01"),
      units = "days"
    )))

  cdm$cohort1d <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "last"
    )
  expect_true(cdm$cohort1d %>%
                dplyr::filter(subject_id == 1) %>%
                dplyr::pull(5) ==
                as.Date("2010-03-25"))
  expect_true(cdm$cohort1d %>%
                dplyr::filter(subject_id == 2) %>%
                dplyr::pull(5) == as.Date("2013-01-03"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("multiple cohort entries per person", {
  # in the presence of multiple cohort entries in the index cohort
  # each record should be treated independently

  cohort1 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1", "1", "2"),
    cohort_start_date = c(
      as.Date("2010-03-01"),
      as.Date("2012-03-01"),
      as.Date("2011-02-01")
    ),
    cohort_end_date = c(
      as.Date("2015-01-01"),
      as.Date("2016-03-01"),
      as.Date("2013-01-01")
    )
  )

  cohort2 <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1", "1", "1", "2"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-15"),
      as.Date("2012-03-25"),
      as.Date("2013-01-03")
    ),
    cohort_end_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-03-15"),
      as.Date("2012-03-25"),
      as.Date("2013-01-03")
    )
  )

  cdm <- mockPatientProfiles(
    cohort1 = cohort1,
    cohort2 = cohort2
  )
  cdm$cohort1 <- addCohortCountAttr(cdm$cohort1)
  cdm$cohort2 <- addCohortCountAttr(cdm$cohort2)

  # 100 days from index
  cdm$cohort1a <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      window = c(0, 100),
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )

  expect_true(all(cdm$cohort1a %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::arrange(cohort_start_date) %>%
    dplyr::pull(5) ==
    c(
      as.numeric(difftime(as.Date("2010-03-03"),
        as.Date("2010-03-01"),
        units = "days"
      )),
      as.numeric(difftime(as.Date("2012-03-25"),
        as.Date("2012-03-01"),
        units = "days"
      ))
    )))

  expect_equal(
    cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull("n"),
    cdm$cohort1a %>% dplyr::tally() %>% dplyr::pull("n")
  )

  cdm$cohort1b <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      window = c(0, 100),
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )

  expect_true(all(cdm$cohort1b %>%
                    dplyr::filter(subject_id == 1) %>%
                    dplyr::arrange(cohort_start_date) %>%
                    dplyr::pull(5) ==
                    c(as.Date("2010-03-03"),
                      as.Date("2012-03-25")
                    )))

  expect_equal(
    cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull("n"),
    cdm$cohort1b %>% dplyr::tally() %>% dplyr::pull("n")
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("output names", {
  # additional column should be added
  # with the name as specified

  cdm <- mockPatientProfiles()
  cdm$cohort1 <- addCohortCountAttr(cdm$cohort1)
  cdm$cohort2 <- addCohortCountAttr(cdm$cohort2)

  cdm$cohort1a <- cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "{id_name}"
    ) # id_name won't be clear to the user
  expect_true(all(c("cohort_1", "cohort_2") %in%
    colnames(cdm$cohort1a)))

  cdm$cohort1b <- cdm$cohort1 %>%
    dateOfCohort(
      cdm = cdm,
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "{id_name}"
    ) # id_name won't be clear to the user
  expect_true(all(c("cohort_1", "cohort_2") %in%
                    colnames(cdm$cohort1b)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors ", {
  cdm <- mockPatientProfiles()

  # not a cdm
  expect_error(cdm$cohort1 %>%
    timeToCohort(
      cdm = "a",
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    ))
  expect_error(cdm$cohort1 %>%
                 dateOfCohort(
                   cdm = "a",
                   targetCohortId = 1,
                   indexDate = "cohort_start_date",
                   targetCohortTable = "cohort2"
                 ))

  # missing outcome table
  expect_error(cdm$cohort1 %>%
    timeToCohort(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "table_x"
    ))
  expect_error(cdm$cohort1 %>%
                 dateOfCohort(
                   cdm = cdm,
                   targetCohortId = 1,
                   indexDate = "cohort_start_date",
                   targetCohortTable = "table_x"
                 ))

  # unreasonable window
  # expect_error(cdm$cohort1 %>%
  #                timeToCohort(cdm = cdm,
  #                            targetCohortId = 1,
  #                            indexDate = "cohort_start_date",
  #                            targetCohortTable = "cohort2",
  #                            window = c(300, 100)))



  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
