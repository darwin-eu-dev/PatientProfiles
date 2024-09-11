test_that("test all functions", {
  x <- dplyr::tibble(
    s = c("g1", "g1", "g2", "g12", "g2", "g12"),
    v_1 = c(1, 2, 3, 4, 6, 3),
    v_2 = c("a", "b", "a", "b", "0", "0"),
    v_3 = c(0, 1, 0, 1, 1, 0),
    v_4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )
  s1 <- summariseResult(x)
  s2 <- summariseResult(x, strata = list("s"))
  s3 <- summariseResult(
    x,
    strata = list("s"),
  )
  s4 <- summariseResult(
    x,
    strata = list(c("s", "v_2"), group2 = "s")
  )

  x <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 1, 2),
    cohort_start_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    cohort_end_date = as.Date(c("1990-04-19", "1991-04-19", "2010-11-14")),
    acetaminophen_m365_to_0 = c(1, 1, 0),
    ibuprophen_m365_to_0 = c(0, 0, 0),
    naloxone_m365_to_0 = c(0, 0, 0),
    headache_minf_to_0 = c(0, 1, 0),
    covid_minf_to_0 = c(1, 1, 0)
  )
  expect_no_error(summariseResult(
    x,
    strata = list()
  ))

  expect_no_error(
    emptySR <- summariseResult(
      x,
      variables = list(),
      estimates = list(),
      counts = FALSE
    )
  )
  expect_true(nrow(emptySR) == 0)
  expect_true(inherits(emptySR, "summarised_result"))

  expect_no_error(
    emptySR <- summariseResult(
      dplyr::tibble(),
      variables = list(),
      estimates = list(),
      counts = FALSE
    )
  )
  expect_true(nrow(emptySR) == 0)
  expect_true(inherits(emptySR, "summarised_result"))

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(39, 40, 27, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(0, 1, 0, 0)
  )
  variables <- c("age", "number_visits", "prior_history", "sex")
  functions <- c(
    "mean", "sd", "median", "q25", "q75", "count_missing", "percentage_missing",
    "count", "percentage", "density"
  )
  expect_no_error(
    result <- summariseResult(
      table = cohort, variables = variables, estimates = functions
    )
  )

  expect_identical(
    dplyr::tibble() |> summariseResult(counts = FALSE),
    omopgenerics::emptySummarisedResult()
  )
})

test_that("groups and strata", {
  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 1000
  )

  result <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(strata = list("sex"))

  expect_true(
    result %>%
      dplyr::filter(
        group_name == "overall" & group_level == "overall" &
          strata_name == "overall" & strata_level == "overall" &
          variable_name == "number subjects"
      ) %>%
      dplyr::pull("estimate_value") |>
      as.numeric() <= 1000
  )

  result <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(strata = list(c("age_group", "sex")))

  expect_true(all(result %>%
    dplyr::select("strata_name") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c("overall", "age_group &&& sex")))
  expect_true(all(result %>%
    dplyr::select("strata_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "overall", "0 to 30 &&& Female", "0 to 30 &&& Male",
      "31 to 60 &&& Female", "31 to 60 &&& Male", "None &&& Female",
      "None &&& Male"
    )))

  result <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect() %>%
    summariseResult(group = c("age_group", "sex"))
  expect_true(all(result %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c("overall", "age_group &&& sex")))
  expect_true(all(result %>%
    dplyr::select("group_level") %>%
    dplyr::distinct() %>%
    dplyr::pull() %in%
    c(
      "overall", "0 to 30 &&& Female", "0 to 30 &&& Male",
      "31 to 60 &&& Female", "31 to 60 &&& Male", "None &&& Female",
      "None &&& Male"
    )))

  expect_no_error(
    result <- cdm$condition_occurrence %>%
      dplyr::collect() %>%
      dplyr::mutate("sex" = "Missing") |>
      summariseResult(group = "sex")
  )

  mockDisconnect(cdm = cdm)
})

test_that("table in db or local", {
  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 1000
  )

  # in db
  expect_no_error(cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    summariseResult(strata = "sex"))

  # already collected
  expect_warning(
    expect_no_error(
      cdm$condition_occurrence %>%
        addDemographics(
          indexDate = "condition_start_date",
          ageGroup = list(c(0, 30), c(31, 60))
        ) %>%
        dplyr::collect() %>%
        dplyr::mutate("subject_id" = .data$person_id) |>
        summariseResult(strata = list("sex"))
    )
  )

  expect_no_error(
    x <- cdm$drug_exposure |>
      addSex() |>
      dplyr::collect() |>
      summariseResult(
        group = "sex", variables = character(), estimates = character()
      )
  )

  mockDisconnect(cdm = cdm)
})

test_that("with and with overall groups and strata", {
  skip_on_cran()
  cdm <- mockPatientProfiles(
    con = connection(), writeSchema = writeSchema(), numberIndividuals = 1000
  )

  test_data <- cdm$condition_occurrence %>%
    addDemographics(
      indexDate = "condition_start_date",
      ageGroup = list(c(0, 30), c(31, 60))
    ) %>%
    dplyr::collect()

  expect_false(any(test_data %>%
    summariseResult(
      strata = list("sex"),
      includeOverallStrata = FALSE
    ) %>%
    dplyr::pull("strata_name") %in%
    c("overall")))
  expect_true(any(test_data %>%
    summariseResult(
      strata = list("sex"),
      includeOverallStrata = TRUE
    ) %>%
    dplyr::pull("strata_name") %in%
    c("overall")))

  expect_false(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = FALSE
    ) %>%
    dplyr::pull("group_name") %in%
    c("overall")))
  expect_true(any(test_data %>%
    summariseResult(
      group = list("sex"),
      includeOverallGroup = TRUE
    ) %>%
    dplyr::pull("group_name") %in%
    c("overall")))

  mockDisconnect(cdm = cdm)
})

test_that("obscure", {
  skip_on_cran()
  x <- dplyr::tibble(
    s = c("g1", "g1", "g2", "g1&&g2", "g2", "g1&&g2"),
    v1 = c(1, 2, 3, 4, 6, 3),
    v2 = c("a", "b", "a&&b", "b", "0", "0&&ab"),
    v3 = c(0, 1, 0, 1, 1, 0),
    v4 = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )

  # minCellCount = 1
  s <- summariseResult(x) |>
    suppress(minCellCount = 1)
  expect_true(nrow(s) == 34)
  expect_true(sum(s$estimate_value[!is.na(s$estimate_value)] == "<1") == 0)
  expect_true(sum(is.na(s$estimate_value)) == 0)

  # minCellCount = 2
  s <- summariseResult(x) |>
    suppress(minCellCount = 2)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 8)

  # minCellCount = 3
  s <- summariseResult(x) |>
    suppress(minCellCount = 3)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 16)

  # minCellCount = 4
  s <- summariseResult(x) |>
    suppress(minCellCount = 4)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 23)

  # minCellCount = 5
  s <- summariseResult(x) |>
    suppress(minCellCount = 5)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 23)

  # minCellCount = 6
  s <- summariseResult(x) |>
    suppress(minCellCount = 6)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 23)

  # minCellCount = 7
  s <- summariseResult(x) |>
    suppress(minCellCount = 7)
  expect_true(nrow(s) == 34)
  expect_true(sum(is.na(s$estimate_value)) == 34)
})

test_that("test empty cohort", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  expect_no_error(
    cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = FALSE,
        includeOverallStrata = TRUE
      )
  )

  expect_no_error(
    cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = TRUE,
        includeOverallStrata = TRUE
      )
  )


  expect_no_error(
    cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = FALSE,
        includeOverallStrata = FALSE
      )
  )

  expect_no_error(
    cdm$cohort1 %>% dplyr::filter(cohort_definition_id == 0) %>%
      summariseResult(
        group = list("cohort_name"),
        includeOverallGroup = TRUE,
        includeOverallStrata = FALSE
      )
  )

  mockDisconnect(cdm = cdm)
})

test_that("test summary table naming", {
  skip_on_cran()
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())

  dat <-
    cdm$cohort1 %>%
    addDemographics() %>%
    dplyr::mutate(
      age_age = age,
      age_age_age = age,
      age_age_age_age = age
    ) %>%
    summariseResult()

  expect_true(all(
    c("age_age", "age", "age_age_age", "age_age_age_age") %in% dat$variable_name
  ))

  mockDisconnect(cdm = cdm)
})

test_that("misisng counts", {
  skip_on_cran()
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(NA, 40, NA, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(NA, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(writeSchema(), "test_table")
  con <- connection()
  DBI::dbWriteTable(con, name = name, value = cohort)
  cohort <- dplyr::tbl(con, name)
  variables <- list(
    numeric = c(
      "age", "number_visits", "prior_history"
    ),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75", "count_missing", "percentage_missing"),
    categorical = c("count", "percentage")
  )
  expect_no_error(
    result <- summariseResult(
      cohort,
      strata = list("sex"), variables = variables,
      estimates = functions
    )
  )
  expected <- dplyr::tribble(
    ~strata, ~variable_name, ~count, ~percentage,
    "overall", "age", 2, 50,
    "overall", "number_visits", 1, 25,
    "overall", "prior_history", 0, 0,
    "Male", "age", 1, 100 / 3,
    "Male", "number_visits", 1, 100 / 3,
    "Male", "prior_history", 0, 0,
    "Female", "age", 1, 100,
    "Female", "number_visits", 0, 0,
    "Female", "prior_history", 0, 0,
  ) %>%
    dplyr::mutate(
      count = as.character(.data$count),
      percentage = as.character(.data$percentage)
    )
  for (k in seq_len(nrow(expected))) {
    x <- result %>%
      dplyr::filter(
        .data$strata_level == .env$expected$strata[k],
        .data$variable_name == .env$expected$variable_name[k]
      )
    xcount <- x$estimate_value[x$estimate_name == "count_missing"]
    xpercentage <- x$estimate_value[x$estimate_name == "percentage_missing"]
    expect_true(xcount == expected$count[k])
    expect_true(xpercentage == expected$percentage[k])
  }
  # female age is all na
  expect_true(
    result %>%
      dplyr::filter(
        .data$variable_name == "age",
        .data$strata_level == "Female",
        is.na(.data$variable_level),
        !.data$estimate_name %in% c("count_missing", "percentage_missing")
      ) %>%
      dplyr::pull("estimate_value") %>%
      is.na() %>%
      all()
  )
  DBI::dbRemoveTable(con, name = name)
  DBI::dbDisconnect(conn = con, shutdown = TRUE)
})

test_that("data is ordered", {
  skip_on_cran()
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(15, 40, 20, 7),
    sex = c("Male", "Male", "Female", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(5, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(writeSchema(), "test_table")
  con <- connection()
  DBI::dbWriteTable(con, name = name, value = cohort)
  testTable <- dplyr::tbl(con, name)
  variables <- list(
    numeric = c("age", "number_visits", "prior_history"),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "percentage", "median")
  )
  expect_no_error(
    result <- summariseResult(
      table = testTable, strata = list("sex"), variables = variables,
      estimates = functions
    )
  )
  # check first overall, second sex
  order <- unique(result$strata_level)
  expect_identical(order, c("overall", "Female", "Male"))
  # first numbers, age, sex, prior_history, number_visits
  variables <- unique(result$variable_name)
  expect_identical(variables, c(
    "number records", "number subjects", "age", "sex", "prior_history",
    "number_visits"
  ))
  # variable levels appear by order
  order <- unique(result$variable_level[result$variable_name == "sex"])
  order <- order[!is.na(order)]
  expect_identical(order, c("Female", "Male"))
  DBI::dbRemoveTable(con, name = name)

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    age = c(15, 40, 20, 7),
    sex = c("Male", "Male", "xFemale", "Male"),
    prior_history = c(365, 25, 14, 48),
    number_visits = c(5, 1, 0, 0)
  )
  name <- CDMConnector::inSchema(writeSchema(), "test_table")
  DBI::dbWriteTable(con, name = name, value = cohort)
  testTable <- dplyr::tbl(con, name)
  variables <- list(
    numeric = c("age", "number_visits", "prior_history"),
    categorical = c("sex")
  )
  functions <- list(
    numeric = c("median", "q25", "q75"),
    categorical = c("count", "percentage", "median")
  )
  expect_no_error(
    result <- summariseResult(
      table = testTable, strata = list("sex"), variables = variables,
      estimates = functions
    )
  )
  # check first overall, second sex
  order <- unique(result$strata_level)
  expect_identical(order, c("overall", "Male", "xFemale"))
  # first numbers, age, sex, prior_history, number_visits
  variables <- unique(result$variable_name)
  expect_identical(variables, c(
    "number records", "number subjects", "age", "sex", "prior_history",
    "number_visits"
  ))
  # variable levels appear by order
  order <- unique(result$variable_level[result$variable_name == "sex"])
  order <- order[!is.na(order)]
  expect_identical(order, c("Male", "xFemale"))
  DBI::dbRemoveTable(con, name = name)

  DBI::dbDisconnect(conn = con, shutdown = TRUE)
})

test_that("NA when min, max and mean works", {
  skip_on_cran()
  # case estimatrs > variables
  expect_no_warning(
    res1 <- dplyr::tibble(group = c("N", "N", "V", "C", "C", "D"), var = c(NA, NA, NA, 1, 1, 1) |> as.integer()) |>
      PatientProfiles::summariseResult(
        group = "group",
        estimates = c("min", "max", "mean", "median", "percentage", "q25")
      )
  )
  expect_equal(
    res1$estimate_value,
    c(
      "2", "1", "2", "1", "100", "100", "100", "100", NA, NA, "1", "1", NA,
      NA, "1", "1", NA, NA, "1", "1", NA, NA, "1", "1", NA, NA,
      "100", "100", NA, NA, "1", "1", "0", "0", "2", "1"
    )
  )

  expect_no_warning(
    res2 <- dplyr::tibble(
      group = c("N", "N", "V", "C", "C", "D"),
      var1 = c(NA, NA, NA, 1, 1, 1) |> as.integer(),
      var2 = c(1, 1, 1, NA, NA, NA) |> as.integer()
    ) |>
      PatientProfiles::summariseResult(
        group = "group",
        estimates = c("min", "max", "mean", "median", "percentage", "q25")
      )
  )
  expect_equal(
    res2$estimate_value,
    c(
      "2", "1", "2", "1", "100", "100", "100", "100", NA, NA, "1", "1", NA,
      NA, "1", "1", NA, NA, "1", "1", NA, NA, "1", "1", NA, NA, "100",
      "100", NA, NA, "1", "1", "0", "0", "2", "1", "1", "1", NA, NA, "1", "1",
      NA, NA, "1", "1", NA, NA, "1", "1", NA, NA, "100", "100", NA,
      NA, "1", "1", NA, NA, "2", "1", "0", "0"
    )
  )

  # case estimatrs <= variables
  expect_no_warning(
    res3 <- dplyr::tibble(
      group = c("N", "N", "V", "C", "C", "D"),
      var1 = c(NA, NA, NA, 1, 1, 1) |> as.integer(),
      var2 = c(1, 1, 1, NA, NA, NA) |> as.integer(),
      var3 = c(NA, NA, NA, 1, 1, 1) |> as.integer()
    ) |>
      PatientProfiles::summariseResult(
        group = "group",
        estimates = c("min", "max", "mean")
      )
  )
  expect_equal(
    res3$estimate_value,
    c(
      "2", "1", "2", "1", NA, NA, "1", "1", NA, NA, "1", "1", NA, NA,
      "1", "1", "1", "1", NA, NA, "1", "1", NA, NA, "1", "1", NA, NA,
      NA, NA, "1", "1", NA, NA, "1", "1", NA, NA, "1", "1"
    )
  )

  # no group no strata
  expect_no_warning(
    res4 <- dplyr::tibble(var1 = c(NA, NA, NA) |> as.integer()) |>
      PatientProfiles::summariseResult(
        estimates = c("min", "max", "mean")
      )
  )
  expect_equal(res4$estimate_value, c("3", NA, NA, NA))
})

test_that("density works correctly", {
  x <- dplyr::tibble(
    sex = c("M", "F", "F", "F", "F", "F"),
    group = c("g1", "g1", "g2", "g12", "g2", "g12"),
    age1 = c(NA, 23, 38, 45, NA, 39),
    age2 = c(20, 23, 38, 45, 67, 39),
    asthma = c(0L, 1L, 0L, 1L, 1L, 0L),
    birth = as.Date(c(
      "2021-05-12", "2012-05-15", "2023-11-30", "2015-12-10", "2014-01-12",
      "1993-04-190"
    ))
  )
  est <- c("density", "mean", "count")
  var <- c("group", "age1", "age2", "asthma", "birth")
  expect_no_error(s <- summariseResult(x, estimates = est, variables = var))
  expect_no_error(s <- summariseResult(
    x, strata = list("sex"), estimates = est, variables = var))
  expect_false("density_x" %in% s$estimate_name[s$variable_name == "group"])
  expect_true("density_x" %in% s$estimate_name[s$variable_name == "age1"])
  expect_true("density_x" %in% s$estimate_name[s$variable_name == "age2"])
  expect_true("density_x" %in% s$estimate_name[s$variable_name == "asthma"])
  expect_true("density_x" %in% s$estimate_name[s$variable_name == "birth"])
  expect_identical(
    unique(s$estimate_type[s$estimate_name == "density_y"]), "numeric"
  )
  expect_identical(
    unique(s$estimate_type[s$estimate_name == "density_x" & s$variable_name == "age1"]),
    "numeric"
  )
  expect_identical(
    unique(s$estimate_type[s$estimate_name == "density_x" & s$variable_name == "age2"]),
    "numeric"
  )
  expect_identical(
    unique(s$estimate_type[s$estimate_name == "density_x" & s$variable_name == "asthma"]),
    "numeric"
  )
  expect_identical(
    unique(s$estimate_type[s$estimate_name == "density_x" & s$variable_name == "birth"]),
    "date"
  )
  sM <- s |>
    dplyr::filter(
      .data$strata_level == "M", startsWith(.data$variable_level, "density")
    )
  expect_identical(unique(sM$estimate_value[sM$estimate_name == "density_y"]), c("0", "1"))
  x <- sM |>
    dplyr::group_by(.data$variable_level) |>
    dplyr::tally()
  expect_true(unique(x$n) == 6L)
  expect_true(length(unique(x$variable_level)) == 3L)
  x <- s |>
    dplyr::filter(
      .data$strata_level == "F", startsWith(.data$variable_level, "density")
    ) |>
    dplyr::group_by(.data$variable_level) |>
    dplyr::tally()
  expect_true(unique(x$n) == 8L)
  expect_true(length(unique(x$variable_level)) == 512L)
})
