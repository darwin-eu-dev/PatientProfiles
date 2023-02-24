test_that("check input length and type for each of the arguments", {
  cdm <-
    mockCohortProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2019-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )

  expect_error(addObservationPeriod("cdm$cohort1", cdm))

  expect_error(addObservationPeriod(cdm$cohort1, "cdm"))

  expect_error(addObservationPeriod(cdm$cohort1, cdm, observationAt = "end_date"))

  expect_error(addObservationPeriod(
    cdm$cohort1,
    cdm,
    observationAt = "cohort_start_date",
    compute = s
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})

test_that("check condition_occurrence and cohort1 work", {

  #mock data
  cdm <-
    mockCohortProfiles(
      seed = 1,
      patient_size = 5,
      latest_observation_start_date = "2005-01-01",
      max_days_to_observation_end = 1,
      min_days_to_observation_end = 1
    )
  #check it works with cohort1 table in mockdb
  expect_true(typeof(cdm$cohort1 %>% addObservationPeriod(cdm) %>% dplyr::collect()) == "list")
  expect_true("observation_period_start_date" %in% colnames(cdm$cohort1 %>% addObservationPeriod(cdm)))
  expect_true("observation_period_end_date" %in% colnames(cdm$cohort1 %>% addObservationPeriod(cdm)))
  #check it works with condition_occurrence table in mockdb
  expect_true(typeof(cdm$condition_occurrence %>% addObservationPeriod(cdm,observationAt = "condition_start_date") %>% dplyr::collect()) == "list")
  expect_true("observation_period_start_date" %in% colnames(cdm$condition_occurrence %>% addObservationPeriod(cdm,observationAt = "condition_start_date")))
  expect_true("observation_period_end_date" %in% colnames(cdm$condition_occurrence %>% addObservationPeriod(cdm,observationAt = "condition_start_date")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("check working example with cohort1",{
    #create mock tables for testing
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

    obs_1 <- tibble::tibble(
      observation_period_id = c("1", "2", "3"),
      person_id = c("1", "2", "3"),
      observation_period_start_date = c(
        as.Date("2010-02-03"),
        as.Date("2010-03-03"),
        as.Date("2010-01-01")
      ),
      observation_period_end_date = c(
        as.Date("2014-01-01"),
        as.Date("2012-01-01"),
        as.Date("2012-01-01")
      )
    )

    cdm <-
      mockCohortProfiles(
        seed = 1,
        cohort1 = cohort1,
        observation_period = obs_1

      )

    result <- cdm$cohort1 %>% addObservationPeriod(cdm) %>% dplyr::collect()

    result2 <- cdm$cohort1 %>% addObservationPeriod(cdm,name = c("a","b"))


    expect_true(all(colnames(cohort1) %in% colnames(result)))

    expect_true(all(
      result %>% dplyr::select("observation_period_start_date") %in% tibble::tibble(observation_period_start_date = c(
        as.Date("2010-02-03"), NA, as.Date("2010-01-01")
      ))
    ))

    expect_true(all(c("a","b") %in% colnames(result2)))


    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE) })


test_that("check working example with condition_occurence", {
      #create mock tables for testing
      condition_occurrence <- tibble::tibble(
        cohort_definition_id = c("1", "1", "1"),
        subject_id = c("1", "2", "3"),
        condition_start_date = c(
          as.Date("2010-03-03"),
          as.Date("2010-03-01"),
          as.Date("2010-02-01")
        ),
        condition_end_date = c(
          as.Date("2015-01-01"),
          as.Date("2013-01-01"),
          as.Date("2013-01-01")
        )
      )

      obs_1 <- tibble::tibble(
        observation_period_id = c("1", "2", "3"),
        person_id = c("1", "2", "3"),
        observation_period_start_date = c(
          as.Date("2010-02-03"),
          as.Date("2010-03-03"),
          as.Date("2010-01-01")
        ),
        observation_period_end_date = c(
          as.Date("2014-01-01"),
          as.Date("2012-01-01"),
          as.Date("2012-01-01")
        )
      )

      cdm <-
        mockCohortProfiles(
          seed = 1,
          condition_occurrence = condition_occurrence,
          observation_period = obs_1

        )

      result <-
        cdm$condition_occurrence %>% addObservationPeriod(cdm, observationAt = "condition_start_date") %>% dplyr::collect()


      expect_true(all(colnames(condition_occurrence) %in% colnames(result)))

      expect_true(all(
        result %>% dplyr::select("observation_period_start_date") %in% tibble::tibble(observation_period_start_date = c(
          as.Date("2010-02-03"), NA, as.Date("2010-01-01")
        ))
      ))


      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    })


test_that("check warning message", {
        #create mock tables for testing
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

        obs_1 <- tibble::tibble(
          observation_period_id = c("1", "2", "3"),
          person_id = c("1", "1", "3"),
          observation_period_start_date = c(
            as.Date("2010-02-03"),
            as.Date("2010-03-03"),
            as.Date("2010-01-01")
          ),
          observation_period_end_date = c(
            as.Date("2014-01-01"),
            as.Date("2012-01-01"),
            as.Date("2012-01-01")
          )
        )

        cdm <-
          mockCohortProfiles(seed = 1,
                             cohort1 = cohort1,
                             observation_period = obs_1)

        expect_warning(addObservationPeriod(x = cdm$cohort1, cdm))


        DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
      })
