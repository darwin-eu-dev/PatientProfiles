test_that("test methods against eunomia duck db", {
  skip_if_not(CDMConnector::eunomia_is_available())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main")

  # create a cohort table to db
  # first use of all drugs from drug_era
  # choice A. generate from SQL.
  # # sql <- SqlRender::readSql(system.file("sql/", "cohortsOfInterest.sql", package = "PatientProfiles"))
  # sql <- SqlRender::readSql("./sql/cohortsOfInterest.sql")
  # sql <- SqlRender::render(sql = sql, cdmDatabaseSchema = "main", resultsDatabaseSchema = "main")
  # sql <- SqlRender::translate(sql = sql, targetDialect = "duckdb")
  # DBI::dbSendQuery(con, sql)
  # cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main", cohort_tables = c("cohort"))

  # choice B.
  cohort_table<-
    cdm$drug_era %>%
    dplyr::group_by(drug_concept_id, person_id) %>%
    dplyr::summarize(cohort_definition_id = first(drug_concept_id),
              cohort_start_date = min(drug_era_start_date),
              cohort_end_date = min(drug_era_end_date),
              subject_id = first(person_id)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(cdm$observation_period,
               by = c("subject_id" = "person_id")) %>%
    dplyr::filter(cohort_start_date >= observation_period_start_date,
           cohort_end_date <= observation_period_end_date) %>%
    dplyr::select(cohort_definition_id, cohort_start_date, cohort_end_date , subject_id) %>%
    dplyr::collect()

  DBI::dbWriteTable(con, "cohort_table", cohort_table, overwrite = TRUE)

  cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main", cohort_tables ="cohort_table" )

  #addAge
    expect_true(typeof(cdm$cohort_table %>% addAge(cdm) %>% dplyr::collect()) == "list")
    expect_true("age" %in% colnames(cdm$cohort_table %>% addAge(cdm)))

    expect_true(typeof(cdm$condition_occurrence %>% addAge(cdm,ageAt = "condition_start_date") %>% dplyr::collect()) == "list")
    expect_true("age" %in% colnames(cdm$condition_occurrence %>% addAge(cdm,ageAt = "condition_start_date")))

    result <- addAge(x = cdm[["cohort_table"]], cdm = cdm, ageAt = "cohort_start_date") %>% dplyr::collect()
    expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "age") %in% names(result)))

  #addAgeGroup
    expect_true(typeof(cdm$cohort_table %>% addAgeGroup(cdm = cdm) %>% dplyr::collect()) == "list")
    expect_true("ageGroupNames" %in% colnames(cdm$cohort_table %>% addAgeGroup(cdm = cdm)))

    expect_true(typeof(cdm$condition_occurrence %>% addAgeGroup(cdm = cdm) %>% dplyr::collect()) == "list")
    expect_true("ageGroupNames" %in% colnames(cdm$condition_occurrence %>% addAgeGroup(cdm = cdm)))

    result <- addAgeGroup(
      x = cdm$cohort_table, ageGroup = list(c(0, 18), c(19, 65), c(66,NA)), cdm = cdm
    ) %>%
      dplyr::collect() %>%
      dplyr::arrange(age)

    expect_true(all(result$ageGroupNames %in% c("0;18","19;65","66;150")))

    expect_error(addAgeGroup(
      x = cdm$cohort_table,
      ageGroup = list(c(1, 2), c(1, 20, 30)),
      cdm = cdm
    ))

    #addSex
    expect_true(typeof(cdm$cohort_table %>% addSex(cdm) %>% dplyr::collect()) == "list")
    expect_true("sex" %in% colnames(cdm$cohort_table %>% addSex(cdm)))
    result <- cdm$cohort_table %>% addSex(cdm) %>%
      dplyr::collect()
    expect_true(all(result$sex %in% c("Male","Female")))

    expect_true(typeof(cdm$condition_occurrence %>% addSex(cdm) %>% dplyr::collect()) == "list")
    expect_true("sex" %in% colnames(cdm$condition_occurrence %>% addSex(cdm)))

    #addDemographics
    result <- cdm$cohort_table %>%
      addDemographics(cdm,
                      demographicsAt = "cohort_end_date",
                      age = TRUE,
                      ageGroup = list(c(0,100)),
                      sex = TRUE,
                      priorHistory = TRUE)
    # age and age group
    expect_true(all(c("age","sex", "ageGroupNames", "prior_history")
                    %in%   names(result)))

    s <- result %>%
      dplyr::filter(
        .data$subject_id == 1000 & .data$cohort_start_date == as.Date("2017-01-29")
      ) %>%
      dplyr::collect()
    expect_true(s$age == 54)
    expect_true(s$sex == "Female")
    expect_true(s$prior_history == 19892)


    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  })
