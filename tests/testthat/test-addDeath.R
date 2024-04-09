test_that("addDeathDate", {
  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)
  cdm$cohort1 <- addDeathDate(x = cdm$cohort1,
                              indexDate = "cohort_start_date",
                              window = c(0, Inf),
                              deathDateName = "ddate")
  expect_true("ddate" %in% colnames(cdm$cohort1))

  cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                              indexDate = "cohort_start_date",
                              window = c(0, Inf),
                              deathDaysName = "ddays")
  expect_true("ddays" %in% colnames(cdm$cohort1))

  cdm$cohort1 <- addDeathFlag(x = cdm$cohort1,
                              indexDate = "cohort_start_date",
                              window = list(c(0, Inf)), #can also provide window as a list
                              deathFlagName = "dflag")
  expect_true("dflag" %in% colnames(cdm$cohort1))


  # warning if variable already exists
  expect_warning(cdm$cohort1 |>
    addDeathFlag(deathFlagName = "dflag2") |>
    addDeathFlag(deathFlagName = "dflag2"))

  # expected errors
  expect_error(addDeathDate(x = "not a table",
              indexDate = "cohort_start_date",
              window = c(0, Inf),
              deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
              indexDate = "not_a_variable",
              window = c(0, Inf),
              deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
                           indexDate = "not_a_variable",
                           window = c(0, 1, 2),
                           deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
              indexDate = "cohort_start_date",
              window = c("not a number", Inf),
              deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
                           indexDate = "cohort_start_date",
                           window = c(10, 2),
                           deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
                           indexDate = "cohort_start_date",
                           window = c(-Inf, -Inf),
                           deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
                           indexDate = "cohort_start_date",
                           window = c(Inf, Inf),
                           deathDateName = "ddate"))
 expect_error(addDeathDate(x = cdm$cohort1,
                           indexDate = "cohort_start_date",
                           window = list(c(0, Inf),c(1, Inf)),
                           deathDateName = "ddate"))
 expect_message(addDeathDate(x = cdm$cohort1,
                           indexDate = "cohort_start_date",
                           window = c(0, Inf),
                           deathDateName = "NotSnakeCase"))

  # no death table in cdm reference
  cdm$death <- NULL
  expect_error(addDeathDate(x = cdm$cohort1))
  expect_error(addDeathDays(x = cdm$cohort1))
  expect_error(addDeathFlag(x = cdm$cohort1))

})

test_that("check alternative index date", {

  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

  # test simple working example
  deathTable <- cdm$cohort1 |>
    dplyr::collect() |>
    dplyr::select("person_id" = "subject_id",
                  "death_date" = "cohort_end_date") |>
    dplyr::filter(person_id == 1L)
  cdm <- omopgenerics::insertTable(cdm = cdm, name ="death", table = deathTable)

  cdm$cohort1 <- addDeathDate(x = cdm$cohort1,
                              indexDate = "cohort_end_date",
                              window = c(0, Inf),
                              deathDateName = "ddate")
  # should be the same date as cohort end date
  local_df <- cdm$cohort1 |>
    dplyr::collect() |>
    dplyr::filter(!is.na(ddate))
  expect_true(all(local_df$cohort_end_date == local_df$ddate))

  cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                              indexDate = "cohort_end_date",
                              window = c(0, Inf),
                              deathDaysName = "ddays")
  local_df <- cdm$cohort1 |>
    dplyr::collect() |>
    dplyr::filter(!is.na(ddays))
  expect_true(all(local_df$ddays == 0))

})

test_that("check window logic", {

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 1, 2, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-01-01")),
    cohort_end_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-06-30"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 1, 1),
    observation_period_start_date = as.Date(c("2015-06-30", "2019-06-30", "2021-06-30")),
    observation_period_end_date = as.Date(c("2018-06-30", "2020-06-30", "2022-06-30")),
    period_type_concept_id = 0
  )
  deathTable <- dplyr::tibble(
    person_id = 1,
    death_date = as.Date("2022-06-30")
  )
  cdm <- mockPatientProfiles(
    connectionDetails,
    cohort1 = cohort1, observation_period = observation_period, cohort2 = cohort1, death = deathTable
  )

  # with window of zero days around cohort end, we should only have death days for last cohort entry
  cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                              indexDate = "cohort_end_date",
                              window = c(0, 0),
                              deathDaysName = "ddays")
 expect_true(cdm$cohort1 |>
    dplyr::filter(!is.na(ddays) & ddays == 0) |>
    dplyr::tally() |>
    dplyr::pull("n") == 1)

 # with window of 1 days to inf for cohort end, we should death days for all but the last cohort entry
 cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                             indexDate = "cohort_end_date",
                             window = c(1, Inf),
                             deathDaysName = "ddays2")
 # only 2 are observed as only events in observation period are considered
 # otherwise 4 would be observed
 expect_true(cdm$cohort1 |>
               dplyr::filter(!is.na(ddays2)) |>
               dplyr::tally() |>
               dplyr::pull("n") == 2)

 # with window of -inf days to inf for cohort end, we should death days for all
 cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                             indexDate = "cohort_end_date",
                             window = c(-Inf, Inf),
                             deathDaysName = "ddays3")
 # only 3 are observed as only events in observation period are considered
 # otherwise 5 would be observed
 expect_true(cdm$cohort1 |>
               dplyr::filter(!is.na(ddays3)) |>
               dplyr::tally() |>
               dplyr::pull("n") == 3)


 # with window of -inf days to -1 for cohort end, we should have no death days for anyone
 cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                             indexDate = "cohort_end_date",
                             window = c(-Inf, -1),
                             deathDaysName = "ddays4")

 expect_true(cdm$cohort1 |>
               dplyr::filter(!is.na(ddays4)) |>
               dplyr::tally() |>
               dplyr::pull("n") == 0)

 # with window of -inf days to 0 for cohort end, we should have death days for last cohort entry
 cdm$cohort1 <- addDeathDays(x = cdm$cohort1,
                             indexDate = "cohort_end_date",
                             window = c(-Inf, 0),
                             deathDaysName = "ddays5")

 expect_true(cdm$cohort1 |>
               dplyr::filter(!is.na(ddays5)) |>
               dplyr::tally() |>
               dplyr::pull("n") == 1)

})

test_that("check with omop table", {

  cdm <- mockPatientProfiles(connectionDetails, seed = 11, patient_size = 10)

  cdm$condition_occurrence <- addDeathDate(x = cdm$condition_occurrence,
                              indexDate = "condition_start_date",
                              window = c(0, Inf),
                              deathDateName = "ddate")
  expect_true("ddate" %in% colnames(cdm$condition_occurrence))

  cdm$condition_occurrence <- addDeathDays(x = cdm$condition_occurrence,
                              indexDate = "condition_start_date",
                              window = c(0, Inf),
                              deathDaysName = "ddays")
  expect_true("ddays" %in% colnames(cdm$condition_occurrence))

  cdm$condition_occurrence <- addDeathFlag(x = cdm$condition_occurrence,
                              indexDate = "condition_start_date",
                              window = list(c(0, Inf)),
                              deathFlagName = "dflag")
  expect_true("dflag" %in% colnames(cdm$condition_occurrence))

  # default index date is cohort start so should error if not changed
   expect_error(addDeathDate(x = cdm$condition_occurrence,
                window = c(0, Inf),
                deathDateName = "ddate"))

})

test_that("check functionality in presence of multiple death records", {

  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 1, 2, 1, 1),
    subject_id = c(1, 1, 1, 1, 1, 2),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-01-01",
                                  "2020-01-01")),
    cohort_end_date = as.Date(c("2020-01-01", "2020-01-01", "2021-07-01", "2021-07-01", "2022-06-30",
                                "2020-01-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4),
    person_id = c(1, 1, 1, 2),
    observation_period_start_date = as.Date(c("2015-06-30", "2019-06-30", "2021-06-30",
                                              "2020-01-01")),
    observation_period_end_date = as.Date(c("2018-06-30", "2020-06-30", "2022-06-30",
                                            "2020-01-01")),
    period_type_concept_id = 0
  )
  cdm <- mockPatientProfiles(
    connectionDetails,
    cohort1 = cohort1, observation_period = observation_period, cohort2 = cohort1
  )
  deathTable <- dplyr::tibble(
    person_id = c(1,1,2),
    death_date = c(as.Date("2022-06-30"), as.Date("2022-07-30"), as.Date("2020-01-01"))
  )
  cdm <- omopgenerics::insertTable(cdm = cdm, name ="death", table = deathTable)

  nrow_start <- cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n")

  cdm$cohort1 <- addDeathDate(x = cdm$cohort1,
                              indexDate = "cohort_end_date",
                              window = c(0, Inf),
                              deathDateName = "death_date")
  nrow_end <- cdm$cohort1 |> dplyr::tally() |> dplyr::pull("n")
  expect_true(nrow_start == nrow_end)

  # all are the first death date for subject 1
  expect_true(all(
    cdm$cohort1 |>
      dplyr::filter(subject_id == 1) |>
      dplyr::select("death_date") |>
      dplyr::distinct() |>
      dplyr::pull() %in%
      as.Date(c("2022-06-30", NA))
  ))

  # now in the last case, starting window from 1 will result in last record having second death date
  cdm$cohort1 <- addDeathDate(x = cdm$cohort1,
                              indexDate = "cohort_end_date",
                              window = c(1, Inf),
                              deathDateName = "death_date_2")
  expect_equal(length(cdm$cohort1 |>
                dplyr::filter(subject_id == 1) |>
                dplyr::select("death_date_2") |>
                dplyr::distinct() |>
                dplyr::pull()), 2)


})
