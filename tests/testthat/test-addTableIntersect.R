test_that("basic structures", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  cdm$ati_visit <- cdm$cohort1 |>
    addTableIntersectCount(
      tableName = "visit_occurrence",
      nameStyle = "{table_name}_{value}_{window_name}"
    ) |>
    addTableIntersectFlag(
      tableName = "visit_occurrence",
      nameStyle = "{table_name}_{value}_{window_name}"
    ) |>
    addTableIntersectDate(
      tableName = "visit_occurrence",
      nameStyle = "{table_name}_{value}_{window_name}"
    ) |>
    addTableIntersectDays(
      tableName = "visit_occurrence",
      nameStyle = "{table_name}_{value}_{window_name}"
    )
  expect_true(all(c(
    "visit_occurrence_flag_0_to_inf", "visit_occurrence_count_0_to_inf",
    "visit_occurrence_date_0_to_inf", "visit_occurrence_days_0_to_inf"
  ) %in% colnames(cdm$ati_visit)))
  mockDisconnect(cdm = cdm)
})

test_that("input validation", {
  cdm <- mockPatientProfiles(con = connection(), writeSchema = writeSchema())
  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      indexDate = "index_date"
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      censorDate = "index_date"
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      censorDate = 42
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      window = c(90, 0)
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      order = 42
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      targetStartDate = "drug_exposure_start_date"
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      targetEndDate = "condition_end_date"
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      flag = 1
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      count = 1
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      date = 1
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      days = 1
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      field = "condition_start_date"
    )))

  expect_error(expect_warning(cdm$cohort1 %>%
    addTableIntersect(
      tableName = "visit_occurrence",
      nameStyle = "table_name_value_window_name"
    )))
  mockDisconnect(cdm = cdm)
})

test_that("addTableIntersectCount example", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1,2,2,2,2,2),
    subject_id = c(1, 1, 1, 1, 1,1,1,1,1,1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01",
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01"
      )
    ),
    cohort_end_date = cohort_start_date + 7
  )

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1:7),
    person_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 2, 2, 2, 3, 3),
    drug_type_concept_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_exposure_start_date = as.Date(
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
    drug_exposure_end_date = as.Date(
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
    quantity = c(20, 10, 3, 5, 12, 44, 9),
    class = c("A", "B", "B", "A", "A", "A", "B")
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    drug_exposure = drug_exposure
  )
  de_count <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectCount(
      tableName = "drug_exposure",
      window = list(c(-50, 50))
    ) %>%
    dplyr::collect()

  expect_identical(
    de_count |> nrow() %>% as.numeric(),
    10
  )

  expect_true("drug_exposure_m50_to_50" %in%
    (colnames(de_count)))

  expect_identical(
    de_count %>% dplyr::filter(cohort_definition_id == 1) %>%
      dplyr::filter(cohort_start_date == "2020-01-01") %>%
      dplyr::pull("drug_exposure_m50_to_50") %>%
      as.numeric(),
    6
  )

  expect_true(all((de_count %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
      dplyr::filter(cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_m50_to_50") %>%
    as.numeric()) == 0))

  expect_identical(
    de_count %>% dplyr::filter(cohort_start_date == "2020-01-01") %>%
      dplyr::filter(cohort_start_date == "2009-01-01") |>
      nrow() |>
      as.numeric(),
    0
  )

  mockDisconnect(cdm = cdm)
})

test_that("addTableIntersectFlag example", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01"
      )
    ),
    cohort_end_date = cohort_start_date + 7
  )

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1:7),
    person_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 2, 2, 2, 3, 3),
    drug_type_concept_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_exposure_start_date = as.Date(
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
    drug_exposure_end_date = as.Date(
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
    quantity = c(20, 10, 3, 5, 12, 44, 9),
    class = c("A", "B", "B", "A", "A", "A", "B")
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    drug_exposure = drug_exposure
  )
  de_flag <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectFlag(
      tableName = "drug_exposure",
      window = list(
        c(-50, 50),
        c(-Inf, 0)
      )
    ) %>%
    dplyr::collect()

  expect_identical(
    de_flag |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_m50_to_50", "drug_exposure_minf_to_0") %in%
    (colnames(de_flag))))

  expect_true(all((de_flag %>% dplyr::select("drug_exposure_m50_to_50") %>%
    dplyr::pull()) %in% c(0, 1)))

  expect_true(all((de_flag %>% dplyr::select("drug_exposure_minf_to_0") %>%
    dplyr::pull()) %in% c(0, 1)))

  expect_identical(
    de_flag %>%
      dplyr::filter(cohort_start_date == "2020-01-01") %>%
      dplyr::pull("drug_exposure_m50_to_50") %>%
      as.numeric(),
    1
  )

  expect_true(all((de_flag %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_m50_to_50") %>%
    as.numeric()) == 0))

  expect_identical(
    de_flag %>%
      dplyr::filter(cohort_start_date == "2020-01-01") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.numeric(),
    0
  )

  expect_true(all((de_flag %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0") %>%
    as.numeric()) == 1))

  mockDisconnect(cdm = cdm)
})

test_that("addTableIntersectDate example", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01"
      )
    ),
    cohort_end_date = cohort_start_date + 7
  )

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1:7),
    person_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 2, 2, 2, 3, 3),
    drug_type_concept_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_exposure_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2021-01-26",
        "2021-01-29",
        "2022-03-15",
        "2022-01-24",
        "2023-02-16"
      )
    ),
    drug_exposure_end_date = as.Date(
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
    quantity = c(20, 10, 3, 5, 12, 44, 9),
    class = c("A", "B", "B", "A", "A", "A", "B")
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    drug_exposure = drug_exposure
  )
  de_date <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectDate(
      tableName = "drug_exposure",
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::collect()

  expect_identical(
    de_date |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_minf_to_0") %in%
    (colnames(de_date))))

  expect_true(is.na(de_date %>%
    dplyr::filter(cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0")))

  expect_true(all(!is.na(de_date %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0"))))

  expect_identical(
    de_date %>%
      dplyr::filter(cohort_start_date == "2022-01-20") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.Date(),
    as.Date("2020-01-15")
  )

  expect_identical(
    de_date %>%
      dplyr::filter(cohort_start_date == "2024-02-01") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.Date(),
    as.Date("2020-01-15")
  )
  de_date2 <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectDate(
      tableName = "drug_exposure",
      window = list(c(-Inf, 0)),
      order = "last"
    ) %>%
    dplyr::collect()

  expect_identical(
    de_date2 |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_minf_to_0") %in%
    (colnames(de_date2))))

  expect_true(is.na(de_date2 %>%
    dplyr::filter(cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0")))

  expect_true(all(!is.na(de_date2 %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0"))))

  expect_identical(
    de_date2 %>%
      dplyr::filter(cohort_start_date == "2022-01-20") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.Date(),
    as.Date("2021-01-29")
  )

  expect_identical(
    de_date2 %>%
      dplyr::filter(cohort_start_date == "2024-02-01") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.Date(),
    as.Date("2023-02-16")
  )

  mockDisconnect(cdm = cdm)
})

test_that("addTableIntersectDays example", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01"
      )
    ),
    cohort_end_date = cohort_start_date + 7
  )

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1:7),
    person_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 2, 2, 2, 3, 3),
    drug_type_concept_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_exposure_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2021-01-26",
        "2021-01-29",
        "2022-03-15",
        "2022-01-24",
        "2023-02-16"
      )
    ),
    drug_exposure_end_date = as.Date(
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
    quantity = c(20, 10, 3, 5, 12, 44, 9),
    class = c("A", "B", "B", "A", "A", "A", "B")
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    drug_exposure = drug_exposure
  )
  de_days <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectDays(
      tableName = "drug_exposure",
      window = list(c(-Inf, 0))
    ) %>%
    dplyr::collect()

  expect_identical(
    de_days |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_minf_to_0") %in%
    (colnames(de_days))))

  expect_true(is.na(de_days %>%
    dplyr::filter(cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0")))

  expect_true(all(!is.na(de_days %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_minf_to_0"))))

  expect_true(all(de_days %>%
    dplyr::filter(!is.na(drug_exposure_minf_to_0)) %>%
    dplyr::select("drug_exposure_minf_to_0") %>%
    dplyr::pull("drug_exposure_minf_to_0") <= 0))

  expect_identical(
    de_days %>%
      dplyr::filter(cohort_start_date == "2024-02-01") %>%
      dplyr::pull("drug_exposure_minf_to_0") %>%
      as.numeric(),
    -as.numeric(as.Date("2024-02-01") - as.Date("2020-01-15"))
  )

  de_days2 <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectDays(
      tableName = "drug_exposure",
      window = list(c(0, Inf)),
      order = "last"
    ) %>%
    dplyr::collect()

  expect_identical(
    de_days2 |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_0_to_inf") %in%
    (colnames(de_days2))))

  expect_true(is.na(de_days2 %>%
    dplyr::filter(cohort_start_date == "2024-02-01") %>%
    dplyr::pull("drug_exposure_0_to_inf")))

  expect_true(all(!is.na(de_days2 %>%
    dplyr::filter(!cohort_start_date == "2024-02-01") %>%
    dplyr::pull("drug_exposure_0_to_inf"))))

  expect_true(all(de_days2 %>%
    dplyr::filter(!is.na(drug_exposure_0_to_inf)) %>%
    dplyr::select("drug_exposure_0_to_inf") %>%
    dplyr::pull("drug_exposure_0_to_inf") >= 0))

  expect_identical(
    de_days2 %>%
      dplyr::filter(cohort_start_date == "2020-01-01") %>%
      dplyr::pull("drug_exposure_0_to_inf") %>%
      as.numeric(),
    as.numeric(as.Date("2023-02-16") - as.Date("2020-01-01"))
  )

  mockDisconnect(cdm = cdm)
})

test_that("addTableIntersectFields example", {
  cohort1 <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c(1, 1, 1, 1, 1),
    cohort_start_date = as.Date(
      c(
        "2020-01-01",
        "2021-01-15",
        "2022-01-20",
        "2023-01-01",
        "2024-02-01"
      )
    ),
    cohort_end_date = cohort_start_date + 7
  )

  drug_exposure <- dplyr::tibble(
    drug_exposure_id = c(1:7),
    person_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_concept_id = c(1, 2, 2, 2, 2, 3, 3),
    drug_type_concept_id = c(1, 1, 1, 1, 1, 1, 1),
    drug_exposure_start_date = as.Date(
      c(
        "2020-01-15",
        "2020-01-25",
        "2021-01-26",
        "2021-01-29",
        "2022-03-15",
        "2022-01-24",
        "2023-02-16"
      )
    ),
    drug_exposure_end_date = as.Date(
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
    quantity = c(20, 10, 3, 5, 12, 44, 9),
    class = c("A", "B", "B", "A", "A", "A", "B")
  )

  cdm <- mockPatientProfiles(
    con = connection(),
    writeSchema = writeSchema(),
    cohort1 = cohort1,
    drug_exposure = drug_exposure
  )

  de_field <- cdm$cohort1 |>
    PatientProfiles::addTableIntersectField(
      tableName = "drug_exposure",
      window = list(c(-Inf, 0)),
      field = "drug_concept_id"
    ) %>%
    dplyr::collect()

  expect_identical(
    de_field |> nrow() %>% as.numeric(),
    5
  )

  expect_true(all(c("drug_exposure_drug_concept_id_minf_to_0") %in%
    (colnames(de_field))))

  expect_true(is.na(de_field %>%
    dplyr::filter(cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_drug_concept_id_minf_to_0")))

  expect_true(all(!is.na(de_field %>%
    dplyr::filter(!cohort_start_date == "2020-01-01") %>%
    dplyr::pull("drug_exposure_drug_concept_id_minf_to_0"))))

  expect_true(all((de_field %>%
    dplyr::filter(!is.na(drug_exposure_drug_concept_id_minf_to_0)) %>%
    dplyr::select("drug_exposure_drug_concept_id_minf_to_0") %>%
    dplyr::pull("drug_exposure_drug_concept_id_minf_to_0") |>
    as.integer()) %in% c(1, 2, 3)))

  mockDisconnect(cdm = cdm)
})
