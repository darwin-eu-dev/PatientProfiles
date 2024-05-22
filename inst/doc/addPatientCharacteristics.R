## ----eval=FALSE---------------------------------------------------------------
#  library(DBI)
#  library(CDMConnector)
#  
#  # The input arguments provided are for illustrative purposes only and do not provide access to any database.
#  
#  con <- DBI::dbConnect(RPostgres::Postgres(),
#    dbname = "omop_cdm",
#    host = "10.80.192.00",
#    user = "user_name",
#    password = "user_pasword"
#  )
#  
#  cdm <- CDMConnector::cdm_from_con(con,
#    cdm_schema = "main",
#    write_schema = "main",
#    cohort_tables = "cohort_example"
#  )

## ----message= FALSE, warning=FALSE--------------------------------------------
library(PatientProfiles)
library(duckdb)
library(dplyr)

cdm <- mockPatientProfiles(
  patient_size = 1000,
  drug_exposure_size = 1000
)

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence %>%
  glimpse()

cdm$condition_occurrence_mod <- cdm$condition_occurrence %>%
  addAge(
    ageDefaultMonth = 1,
    ageDefaultDay = 6,
    indexDate = "condition_start_date",
    ageGroup = list(
      "age_band_20" =
        list(
          "0 to 19" = c(0, 19),
          "20 to 39" = c(20, 39),
          "40 to 59" = c(40, 59),
          "60 to 79" = c(60, 79),
          "80 to 99" = c(80, 99),
          ">= 100" = c(100, 150)
        ),
      "age_threshold_60" =
        list(
          "less60" = c(0, 59),
          "more60" = c(60, 150)
        )
    )
  ) |>
  dplyr::compute(name = "condition_occurrence_mod")

cdm$condition_occurrence_mod %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$condition_occurrence_mod <- cdm$condition_occurrence_mod %>%
  addSex()

cdm$condition_occurrence_mod %>%
  glimpse()

numConditions <- cdm$condition_occurrence_mod %>%
  filter(
    sex == "Male"
  ) %>%
  filter(
    age_threshold_60 == "more60"
  ) %>%
  filter(
    condition_concept_id == 5
  ) %>%
  group_by(
    age_band_20
  ) %>%
  summarise(
    n = count(condition_occurrence_id)
  )

numConditions

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort1 %>%
  glimpse()

cdm$cohort1 <- cdm$cohort1 %>%
  addInObservation() %>%
  filter(
    in_observation == 1
  ) %>%
  addPriorObservation() %>%
  addFutureObservation()

cdm$cohort1 %>%
  glimpse()

## ----message= FALSE, warning=FALSE--------------------------------------------
cdm$cohort2 %>%
  glimpse()

cdm$cohort2 <- cdm$cohort2 %>%
  addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup = NULL,
    sex = TRUE,
    sexName = "sex",
    priorObservation = TRUE,
    priorObservationName = "prior_observation",
    futureObservation = FALSE,
  )

cdm$cohort2 %>%
  glimpse()

