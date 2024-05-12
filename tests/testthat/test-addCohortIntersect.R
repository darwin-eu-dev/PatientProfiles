
test_that("output format - one outcome cohort", {
  # output format - one outcome cohort ----
  # additional column should be added
  # with the name as specified

  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 10,
    seed = 1
  )

  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectDays(
      targetCohortId = 1,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )

  expect_true(ncol(cdm$cohort1a) == 5)

  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
      targetCohortId = 1,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1b) == 5)

  # output format - multiple outcome cohorts ----
  # additional columns (one per outcome cohort) should be added
  # with the name as specified

  # In 0 to Inf - 2 target cohorts have someone
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectDays(
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1a) == 7)
  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
      window = c(0, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1b) == 7)

  # In -Inf to Inf - 2 target cohorts have someone
  cdm$cohort1c <- cdm$cohort1 %>%
    addCohortIntersectDays(
      window = c(-Inf, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1c) == 7)
  cdm$cohort1d <- cdm$cohort1 %>%
    addCohortIntersectDate(
      window = c(-Inf, Inf),
      targetCohortId = NULL,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(ncol(cdm$cohort1d) == 7)

  mockDisconnect(cdm)
})

test_that("first vs last event - cohort table", {
  # depending on user choice, should get back either the
  # first or last outcome record

  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1L, 2L),
    cohort_start_date = c(as.Date("2010-03-01"), as.Date("2011-02-01")),
    cohort_end_date = c(as.Date("2015-01-01"), as.Date("2013-01-01"))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1L, 1L, 1L, 2L),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-15"), as.Date("2010-03-25"),
      as.Date("2013-01-03")
    ),
    cohort_end_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-15"), as.Date("2010-03-25"),
      as.Date("2013-01-03")
    )
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 2
  )

  # first
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectDays(
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
    addCohortIntersectDays(
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

  mockDisconnect(cdm)
})

test_that("multiple cohort entries per person", {
  # in the presence of multiple cohort entries in the index cohort
  # each record should be treated independently

  cohort1 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1L, 1L, 2L),
    cohort_start_date = c(
      as.Date("2010-03-01"), as.Date("2012-03-01"), as.Date("2011-02-01")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"), as.Date("2016-03-01"), as.Date("2013-01-01")
    )
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(1L, 1L, 1L, 2L),
    cohort_start_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-15"), as.Date("2012-03-25"),
      as.Date("2013-01-03")
    ),
    cohort_end_date = c(
      as.Date("2010-03-03"), as.Date("2010-03-15"), as.Date("2012-03-25"),
      as.Date("2013-01-03")
    )
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 2
  )

  # 100 days from index
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectDays(
      window = c(0, 100),
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )

  expect_true(all(cdm$cohort1a %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::collect() |>
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
      window = c(0, 100),
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      order = "first"
    )

  expect_true(all(cdm$cohort1b %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::collect() |>
    dplyr::arrange(cohort_start_date) %>%
    dplyr::pull(5) ==
    c(
      as.Date("2010-03-03"),
      as.Date("2012-03-25")
    )))

  expect_equal(
    cdm$cohort1 %>% dplyr::tally() %>% dplyr::pull("n"),
    cdm$cohort1b %>% dplyr::tally() %>% dplyr::pull("n")
  )

  mockDisconnect(cdm)
})

test_that("output names", {
  # additional column should be added
  # with the name as specified

  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 10,
    seed = 1
  )

  # default naming
  cdm$cohort1a <- cdm$cohort1 %>%
    addCohortIntersectDays(
      window = c(10, 50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    )
  expect_true(all(
    c("cohort_1_10_to_50", "cohort_2_10_to_50", "cohort_3_10_to_50") %in%
      colnames(cdm$cohort1a)
  ))

  cdm$cohort1b <- cdm$cohort1 %>%
    addCohortIntersectDate(
      window = c(10, 50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2"
    ) # id_name won't be clear to the user
  expect_true(all(
    c("cohort_1_10_to_50", "cohort_2_10_to_50", "cohort_3_10_to_50") %in%
      colnames(cdm$cohort1b)
  ))

  # new names
  cdm$cohort1c <- cdm$cohort1 %>%
    addCohortIntersectDays(
      window = c(10, 50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "study_{cohort_name}"
    )
  expect_true(all(
    c("study_cohort_1", "study_cohort_2", "study_cohort_3") %in%
      colnames(cdm$cohort1c)
  ))

  # new names
  cdm$cohort1d <- cdm$cohort1 %>%
    addCohortIntersectDate(
      window = c(10, 50),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "study_{cohort_name}"
    )
  expect_true(all(
    c("study_cohort_1", "study_cohort_2", "study_cohort_3") %in%
      colnames(cdm$cohort1c)
  ))

  # bad naming
  expect_error(cdm$cohort1 %>%
    addCohortIntersectDate(
      window = list(c(0, 3), c(10, 50)),
      targetCohortId = NULL,
      targetDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      nameStyle = "study"
    ))

  mockDisconnect(cdm)
})

test_that("expected errors ", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  # missing outcome table
  expect_error(cdm$cohort1 %>%
    addCohortIntersectDays(
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "table_x"
    ))
  expect_error(cdm$cohort1 %>%
    addCohortIntersectDate(
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "table_x"
    ))

  expect_error(cdm$cohort1 %>%
    addCohortIntersectDays(
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      window = c(300, 100)
    ))

  expect_error(cdm$cohort1 %>%
    addCohortIntersectDate(
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      censorDate = as.Date("2020-01-01")
    ))

  expect_error(cdm$cohort1 %>%
    addCohortIntersectDate(
      targetCohortId = 1,
      indexDate = "cohort_start_date",
      targetCohortTable = "cohort2",
      censorDate = "subject_id"
    ))

  mockDisconnect(cdm)
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

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 2
  )

  result0 <- cdm$cohort1 %>%
    addCohortIntersectCount(targetCohortTable = "cohort2") %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result1 <- cdm$cohort1 %>%
    addCohortIntersectCount(targetCohortTable = "cohort2", targetCohortId = 1) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result2 <- cdm$cohort1 %>%
    addCohortIntersectCount(targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result3 <- cdm$cohort1 %>%
    addCohortIntersectCount(targetCohortTable = "cohort2", targetCohortId = 3) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(result0$cohort_1_0_to_inf == result1$cohort_1_0_to_inf))
  expect_true(all(result0$cohort_2_0_to_inf == result2$cohort_2_0_to_inf))
  expect_true(all(result0$cohort_3_0_to_inf == result3$cohort_3_0_to_inf))

  result1 <- cdm$cohort1 %>%
    addCohortIntersectCount(
      targetCohortTable = "cohort2", targetCohortId = c(2, 3),
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(result1$cohort_2_minf_to_0 == c(0, 0, 0, 0, 1)))
  expect_true(all(result1$cohort_3_minf_to_0 == c(0, 0, 0, 0, 1)))

  attr(cdm$cohort2, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3),
    cohort_name = c("asthma", "covid", "tb")
  )
  result2 <- cdm$cohort1 %>%
    addCohortIntersectCount(
      targetCohortTable = "cohort2", targetCohortId = c(2, 3),
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(result2$covid_minf_to_0 == c(0, 0, 0, 0, 1)))
  expect_true(all(result2$tb_minf_to_0 == c(0, 0, 0, 0, 1)))

  mockDisconnect(cdm)
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

  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 2
  )

  result0 <- cdm$cohort1 %>%
    addCohortIntersectFlag(targetCohortTable = "cohort2") %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result1 <- cdm$cohort1 %>%
    addCohortIntersectFlag(targetCohortTable = "cohort2", targetCohortId = 1) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result2 <- cdm$cohort1 %>%
    addCohortIntersectFlag(targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)
  result3 <- cdm$cohort1 %>%
    addCohortIntersectFlag(targetCohortTable = "cohort2", targetCohortId = 3) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(result0$cohort_1_0_to_inf == result1$cohort_1_0_to_inf))
  expect_true(all(result0$cohort_2_0_to_inf == result2$cohort_2_0_to_inf))
  expect_true(all(result0$cohort_3_0_to_inf == result3$cohort_3_0_to_inf))

  result1 <- cdm$cohort1 %>%
    addCohortIntersectFlag(targetCohortTable = "cohort2", targetCohortId = 2) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(result1$cohort_2_0_to_inf == c(1, 1, 1, 1, 0)))

  mockDisconnect(cdm)
})

test_that("working examples", {
  # functionality
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-01-15", "2020-01-20", "2020-01-01", "2020-02-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-01", "2020-01-15", "2020-01-20", "2020-01-01", "2020-02-01"
    ))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 2, 3, 3, 3),
    subject_id = c(1, 1, 1, 2, 2, 2, 1),
    cohort_start_date = as.Date(c(
      "2020-01-15", "2020-01-25", "2020-01-26", "2020-01-29", "2020-03-15",
      "2020-01-24", "2020-02-16"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-15", "2020-01-25", "2020-01-26", "2020-01-29", "2020-03-15",
      "2020-01-24", "2020-02-16"
    ))
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 2
  )

  expect_no_error(
    result2 <- cdm$cohort1 %>%
      addCohortIntersectCount(
        targetCohortTable = "cohort2",
        nameStyle = "{value}_{cohort_name}_{window_name}"
      ) %>%
      addCohortIntersectFlag(
        targetCohortTable = "cohort2",
        nameStyle = "{value}_{cohort_name}_{window_name}"
      ) %>%
      addCohortIntersectDate(
        targetCohortTable = "cohort2",
        nameStyle = "{value}_{cohort_name}_{window_name}"
      ) %>%
      addCohortIntersectDays(
        targetCohortTable = "cohort2",
        nameStyle = "{value}_{cohort_name}_{window_name}"
      ) %>%
      dplyr::collect() %>%
      dplyr::arrange(subject_id, cohort_start_date)
  )

  mockDisconnect(cdm)
})

test_that("censorDate functionality", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 2, 3, 4, 5),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-01-15", "2020-01-20", "2020-01-01", "2020-02-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-03-01", "2021-01-15", "2022-01-20", "2020-01-06", "2020-07-01"
    ))
  )

  cohort2 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c(1, 1, 2, 3, 4, 5, 5),
    cohort_start_date = as.Date(c(
      "2020-01-15", "2020-01-25", "2020-01-26", "2020-01-29", "2020-03-15",
      "2020-01-24", "2020-02-16"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-15", "2020-01-25", "2020-01-26", "2020-01-29", "2020-03-15",
      "2020-01-24", "2020-02-16"
    ))
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    cohort2 = cohort2,
    numberIndividuals = 5
  )

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  result1 <- cdm$cohort1 %>%
    addCohortIntersectFlag(
      targetCohortTable = "cohort2",
      censorDate = "cohort_end_date",
      nameStyle = "{value}_{window_name}"
    ) %>%
    addCohortIntersectCount(
      targetCohortTable = "cohort2",
      censorDate = "cohort_end_date",
      nameStyle = "{value}_{window_name}"
    ) %>%
    addCohortIntersectDate(
      targetCohortTable = "cohort2",
      censorDate = "cohort_end_date",
      nameStyle = "{value}_{window_name}"
    ) %>%
    addCohortIntersectDays(
      targetCohortTable = "cohort2",
      censorDate = "cohort_end_date",
      nameStyle = "{value}_{window_name}"
    ) %>%
    dplyr::collect() %>%
    dplyr::arrange(subject_id, cohort_start_date)

  expect_true(all(compareNA(
    result1 %>% dplyr::filter(subject_id == 4) %>%
      dplyr::select(dplyr::ends_with("inf")) %>% dplyr::arrange("subject_id") %>%
      unlist(use.names = F),
    c(0, 0, NA, NA)
  )))

  mockDisconnect(cdm)
})

test_that("casing of empty dates", {
  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 3,
    seed = 1
  )
  cdm$cohort1 <- cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 1)
  expect_false(
    cdm$cohort2 %>%
      addCohortIntersectDate(targetCohortTable = "cohort1") %>%
      head(1) %>%
      dplyr::pull("cohort_2_0_to_inf") %>%
      is.numeric()
  )

  mockDisconnect(cdm)
})

test_that("cohortIntersect after observation", {
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = as.Date(c("2020-01-01", "2020-06-01")),
      cohort_end_date = as.Date(c("2020-04-01", "2020-08-01"))
    ),
    cohort2 = dplyr::tibble(
      cohort_definition_id = c(1L, 2L, 1L),
      subject_id = 1L,
      cohort_start_date = as.Date(c("2019-12-30", "2020-05-25", "2020-05-25")),
      cohort_end_date = as.Date(c("2019-12-30", "2020-05-25", "2020-05-25"))
    ),
    person = dplyr::tibble(
      person_id = 1L,
      gender_concept_id = 8532L,
      year_of_birth = 1992L,
      month_of_birth = 12L,
      day_of_birth = 30L,
      race_concept_id = 0L,
      ethnicity_concept_id = 0L
    ),
    observation_period = dplyr::tibble(
      observation_period_id = 1L,
      person_id = 1L,
      observation_period_start_date = as.Date("2006-03-11"),
      observation_period_end_date = as.Date("2102-04-02"),
      period_type_concept_id = 0L
    )
  )

  windows <- list(
    c(-Inf, Inf), c(0, 0), c(0, Inf), c(5000, 31000), c(31000, Inf),
    c(31000, 45000), c(-Inf, -5000), c(-Inf, -6000), c(-8000, -6000)
  )

  expect_no_error(
    x <- cdm$cohort1 |>
      addCohortIntersectFlag(
        targetCohortTable = "cohort2",
        targetCohortId = 1,
        window = windows,
        nameStyle = "flag_{window_name}"
      ) |>
      addCohortIntersectCount(
        targetCohortTable = "cohort2",
        targetCohortId = 1,
        window = windows,
        nameStyle = "count_{window_name}"
      ) |>
      addCohortIntersectDays(
        targetCohortTable = "cohort2",
        targetCohortId = 1,
        window = windows,
        nameStyle = "days_{window_name}"
      ) |>
      addCohortIntersectDate(
        targetCohortTable = "cohort2",
        targetCohortId = 1,
        window = windows,
        nameStyle = "date_{window_name}"
      ) |>
      dplyr::collect()
  )

  windows <- checkWindow(windows)
  out <- c(5, 6, 8, 9)
  for (k in seq_along(windows)) {
    for (val in c("flag", "count", "date", "days")) {
      col <- paste0(val, "_", names(windows)[k])
      expect_true(col %in% colnames(x))
      if (k %in% out) {
        expect_true(all(is.na(x[[col]])))
      } else if (val %in% c("flag", "count")) {
        expect_true(all(!is.na(x[[col]])))
      }
    }
  }

  mockDisconnect(cdm)
})

test_that("issue 612", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 1, 2, 3, 1, 2),
    subject_id = c(1, 1, 1, 2, 3, 3, 4, 4),
    cohort_start_date = as.Date(c(
      "2020-03-01", "2020-04-01", "2020-01-01", "2020-02-01", "2020-03-01",
      "2020-04-01", "2020-02-01", "2020-06-01"
    )),
    cohort_end_date = as.Date(c(
      "2020-05-01", "2020-06-01", "2020-05-01", "2020-05-01", "2020-05-01",
      "2020-07-01", "2020-02-04", "2020-06-08"
    ))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4),
    gender_concept_id = c(8507, 8532, 8507, 8532),
    year_of_birth = 2000,
    month_of_birth = 1,
    day_of_birth = 1,
    race_concept_id = NA_character_,
    ethnicity_concept_id = NA_character_
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2020-12-31"),
    period_type_concept_id = 32880
  )
  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    observation_period = observation_period,
    person = person,
    cohort1 = cohort
  )

  x <- cdm$cohort1 |>
    addCohortIntersectFlag(
      targetCohortTable = "cohort1",
      window = c(0, 0),
      nameStyle = "{cohort_name}"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(
      .data$cohort_definition_id, .data$subject_id, .data$cohort_start_date
    )

  expect_true(all(x$cohort_1 == c(1, 1, 1, 1, 0, 0, 0, 0)))
  expect_true(all(x$cohort_2 == c(0, 0, 0, 1, 1, 1, 0, 1)))
  expect_true(all(x$cohort_3 == c(1, 0, 0, 1, 0, 0, 1, 1)))

  mockDisconnect(cdm)
})
