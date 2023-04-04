test_that("test addDemographics related", {
  skip_if_not(CDMConnector::eunomia_is_available())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main")

  # create a cohort table to db
  # first use of all drugs from drug_era

  cohort_table<-
    cdm$drug_era %>%
    dplyr::filter(drug_concept_id %in% c(1125315,1112807,1713332,1759842,1729720,1177480,738818,1119510,1118084, 19127890)) %>%
    dplyr::group_by(drug_concept_id, person_id) %>%
    dplyr::summarize(cohort_definition_id = first(drug_concept_id),
              cohort_start_date = min(drug_era_start_date,na.rm = TRUE),
              cohort_end_date = min(drug_era_end_date,na.rm = TRUE),
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

    expect_true(typeof(cdm$condition_occurrence %>% addAge(cdm,indexDate  = "condition_start_date") %>% dplyr::collect()) == "list")
    expect_true("age" %in% colnames(cdm$condition_occurrence %>% addAge(cdm,indexDate  = "condition_start_date")))

    result <- addAge(x = cdm[["cohort_table"]], cdm = cdm, indexDate  = "cohort_start_date") %>% dplyr::collect()
    expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "age") %in% names(result)))

  #addCategories in age
    categories <- list("age_group" = list(c(0, 18), c(19, 65), c(66,150)))

    expect_true(typeof(cdm$cohort_table %>% addAge(cdm) %>%
                         addCategories(cdm, "age", categories) %>% dplyr::collect()) == "list")

    expect_true("age_group" %in% colnames(cdm$cohort_table %>% addAge(cdm) %>% addCategories(cdm, "age", categories)))

    expect_true(typeof(cdm$condition_occurrence %>% addAge(cdm, indexDate = "condition_start_date") %>% addCategories(cdm, "age", categories) %>% dplyr::collect()) == "list")
    expect_true("age_group" %in% colnames(cdm$condition_occurrence %>%
                                            addAge(cdm, indexDate = "condition_start_date") %>%
                                            addCategories(cdm, "age", categories)))

    #addSex
    expect_true(typeof(cdm$cohort_table %>% addSex(cdm) %>% dplyr::collect()) == "list")
    expect_true("sex" %in% colnames(cdm$cohort_table %>% addSex(cdm)))
    result <- cdm$cohort_table %>% addSex(cdm) %>%
      dplyr::collect()
    expect_true(all(result$sex %in% c("Male","Female")))

    expect_true(typeof(cdm$condition_occurrence %>% addSex(cdm) %>% dplyr::collect()) == "list")
    expect_true("sex" %in% colnames(cdm$condition_occurrence %>% addSex(cdm)))


    #addFutureObservation
    expect_true("future_observation" %in% colnames(cdm$cohort_table %>% addFutureObservation(cdm)))
    expect_true("future_observation" %in% colnames(cdm$condition_occurrence %>% addFutureObservation(cdm,indexDate = "condition_start_date")))

    result <- cdm$cohort_table %>% addFutureObservation(cdm,indexDate  = "cohort_end_date", futureObservationName = "timeToExit")
    expect_true("timeToExit" %in% colnames(result))


    #addDemographics
    result <- cdm$cohort_table %>%
      addDemographics(cdm,
                      indexDate  = "cohort_end_date",
                      ageGroup = list("age_group" = list(c(0, 65), c(66, 120))))
    # age and age group
    expect_true(all(c("age","sex", "age_group", "prior_history","future_observation")
                    %in%   names(result)))

    s <- result %>%
      dplyr::filter(
        .data$subject_id == 99 & .data$cohort_definition_id == 1119510       ) %>%
      dplyr::collect()
    expect_true(nrow(s) == 1)
    expect_true(s$age == 57)
    expect_true(s$sex == "Female")
    expect_true(s$prior_history == 20615)

    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  })

test_that("test addIntersect related", {
  skip_if_not(CDMConnector::eunomia_is_available())
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main")

  # create a cohort table to db
  # first use of all drugs from drug_era

  #countOccurrences
  cohort_table<-
    cdm$drug_era %>%
    dplyr::filter(drug_concept_id %in% c(1713332,1759842,1729720,1177480 )) %>%
    dplyr::group_by(drug_concept_id, person_id) %>%
    dplyr::summarize(cohort_definition_id = first(drug_concept_id),
                     cohort_start_date = min(drug_era_start_date,na.rm = TRUE),
                     cohort_end_date = min(drug_era_end_date,na.rm = TRUE),
                     subject_id = first(person_id)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(cdm$observation_period,
                      by = c("subject_id" = "person_id")) %>%
    dplyr::filter(cohort_start_date >= observation_period_start_date,
                  cohort_end_date <= observation_period_end_date) %>%
    dplyr::select(cohort_definition_id, cohort_start_date, cohort_end_date , subject_id)


  cohort_table_1 <- cohort_table %>% dplyr::filter(cohort_definition_id %in% c(1713332,1759842 ))  %>% dplyr::collect()
  cohort_table_2 <- cohort_table %>% dplyr::filter(cohort_definition_id %in% c(1729720,1177480 ))  %>% dplyr::collect()

  DBI::dbWriteTable(con, "cohort_table_1", cohort_table_1, overwrite = TRUE)
  DBI::dbWriteTable(con, "cohort_table_2", cohort_table_2, overwrite = TRUE)

  cdm <- CDMConnector::cdm_from_con(con, "main", write_schema = "main", cohort_tables = c("cohort_table_1","cohort_table_2") )

  result <- cdm$cohort_table_1 %>%
    countOccurrences(cdm = cdm, tableName = "cohort_table_2",cohortId = 1729720 ,window = list(c(0, -Inf))) %>%
    dplyr::arrange(subject_id, cohort_start_date) %>%
    dplyr::collect()


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
