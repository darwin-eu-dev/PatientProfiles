test_that("output format - one outcome cohort", {
  # additional column should be added
  # with the name as specified

  cdm <- mockPatientProfiles()

  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectTime(
      cdm = cdm,
      targetCohortId = 1,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1a) == 5)

  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
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

  # In 0 to Inf - 2 target cohorts have someone
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectTime(
      cdm = cdm,
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1a) == 7)
  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm,
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1b) == 7)

  # In -Inf to Inf - 2 target cohorts have someone
  cdm$cohort1c <- cdm$cohort1 %>%
    addCohortIntersectTime(
      cdm = cdm,
      window = c(-Inf, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1c) == 7)
  cdm$cohort1d <- cdm$cohort1 %>%
    addCohortIntersectDate(
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

  # first
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectTime(
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
    addCohortIntersectDate(
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
    addCohortIntersectTime(
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
    addCohortIntersectDate(
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

  # 100 days from index
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectTime(
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
    addCohortIntersectDate(
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

  # default naming
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectTime(
      cdm = cdm,
      window = c(10,50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(all(c("cohort_1_10_to_50",
                    "cohort_2_10_to_50",
                    "cohort_3_10_to_50") %in%
    colnames(cdm$cohort1a)))

  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm,
      window = c(10,50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    ) # id_name won't be clear to the user
  expect_true(all(c("cohort_1_10_to_50",
                    "cohort_2_10_to_50",
                    "cohort_3_10_to_50") %in%
                    colnames(cdm$cohort1b)))

  # new names
  cdm$cohort1c <- cdm$cohort1 %>%
    addCohortIntersectTime(
      cdm = cdm,
      window = c(10,50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "study_{cohort_name}"
    )
  expect_true(all(c("study_cohort_1",
                    "study_cohort_2",
                    "study_cohort_3") %in%
                    colnames(cdm$cohort1c)))

  # new names
  cdm$cohort1d <- cdm$cohort1 %>%
    addCohortIntersectDate(
      cdm = cdm,
      window = c(10,50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "study_{cohort_name}"
    )
  expect_true(all(c("study_cohort_1",
                    "study_cohort_2",
                    "study_cohort_3") %in%
                    colnames(cdm$cohort1c)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors ", {
  cdm <- mockPatientProfiles()

  # not a cdm
  expect_error(cdm$cohort1 %>%
                 addCohortIntersectTime(
      cdm = "a",
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    ))
  expect_error(cdm$cohort1 %>%
                 addCohortIntersectDate(
                   cdm = "a",
                   targetCohortId = 1,
                   indexDate = "cohort_start_date",
                   targetCohortTable = "cohort2"
                 ))

  # missing outcome table
  expect_error(cdm$cohort1 %>%
                 addCohortIntersectTime(
      cdm = cdm,
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "table_x"
    ))
  expect_error(cdm$cohort1 %>%
                 addCohortIntersectDate(
                   cdm = cdm,
                   targetCohortId = 1,
                   indexDate = "cohort_start_date",
                   targetCohortTable = "table_x"
                 ))

  # unreasonable window
  # expect_error(cdm$cohort1 %>%
  #                addCohortIntersectTime(cdm = cdm,
  #                            targetCohortId = 1,
  #                            indexDate = "cohort_start_date",
  #                            targetCohortTable = "cohort2",
  #                            window = c(300, 100)))



  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples", {

  # functionality
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result0 <- cdm$cohort1 %>%
    addCohortIntersectCount(cdm = cdm, targetCohortTable = "cohort2") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result1 <- cdm$cohort1 %>%
    addCohortIntersectCount(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 1) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result2 <- cdm$cohort1 %>%
    addCohortIntersectCount(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result3 <- cdm$cohort1 %>%
    addCohortIntersectCount(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 3) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result0$cohort_1_0_to_inf == result1$cohort_1_0_to_inf))
  expect_true(all(result0$cohort_2_0_to_inf == result2$cohort_2_0_to_inf))
  expect_true(all(result0$cohort_3_0_to_inf == result3$cohort_3_0_to_inf))

  result_1 <- cdm$cohort1 %>%
    addCohortIntersectCount(
      cdm = cdm, targetCohortTable = "cohort2", targetCohortId = c(2, 3),
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$cohort_2_minf_to_0 == c(0, 0, 0, 0, 1)))
  expect_true(all(result_1$cohort_3_minf_to_0 == c(0, 0, 0, 0, 1)))

  attr(cdm$cohort2, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("asthma", "covid", "tb")
  )
  result_2 <- cdm$cohort1 %>%
    addCohortIntersectCount(
      cdm = cdm, targetCohortTable = "cohort2", targetCohortId = c(2, 3),
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_2$covid_minf_to_0 == c(0, 0, 0, 0, 1)))
  expect_true(all(result_2$tb_minf_to_0 == c(0, 0, 0, 0, 1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples", {

  # functionality
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result0 <- cdm$cohort1 %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result1 <- cdm$cohort1 %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 1) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result2 <- cdm$cohort1 %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()
  result3 <- cdm$cohort1 %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 3) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result0$cohort_1_0_to_inf == result1$cohort_1_0_to_inf))
  expect_true(all(result0$cohort_2_0_to_inf == result2$cohort_2_0_to_inf))
  expect_true(all(result0$cohort_3_0_to_inf == result3$cohort_3_0_to_inf))

  result_1 <- cdm$cohort1 %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  expect_true(all(result_1$cohort_2_0_to_inf == c(1, 1, 1, 1, 0)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("working examples", {

  # functionality
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

  cdm <- mockPatientProfiles(cohort1 = cohort1, cohort2 = cohort2)

  result1 <- cdm$cohort1 %>%
    addCohortIntersect(cdm = cdm, targetCohortTable = "cohort2") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  result2 <- cdm$cohort1 %>%
    addCohortIntersectCount(cdm = cdm, targetCohortTable = "cohort2", nameStyle = "{value}_{cohort_name}_{window_name}") %>%
    addCohortIntersectFlag(cdm = cdm, targetCohortTable = "cohort2", nameStyle = "{value}_{cohort_name}_{window_name}") %>%
    addCohortIntersectDate(cdm = cdm, targetCohortTable = "cohort2", nameStyle = "{value}_{cohort_name}_{window_name}") %>%
    addCohortIntersectTime(cdm = cdm, targetCohortTable = "cohort2", nameStyle = "{value}_{cohort_name}_{window_name}") %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()

  for (k in colnames(result1)) {
    x <- result1[[k]]
    x <- x[!is.na(x)]
    y <- result2[[k]]
    y <- y[!is.na(y)]
    expect_true(all(x == y))
  }

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
