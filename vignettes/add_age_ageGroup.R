## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
library(PatientProfiles)

## ---- eval=FALSE--------------------------------------------------------------
#  library(DBI)
#  library(CDMConnector)
#  library(PatientProfiles)
#  con <- DBI::dbConnect(RPostgres::Postgres(),
#                        dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
#                        host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
#                        user = Sys.getenv("CDM5_POSTGRESQL_USER"),
#                        password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
#  cdm <- CDMConnector::cdm_from_con(con,
#                                    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"))

## ---- message= FALSE, warning=FALSE-------------------------------------------
library(DBI)
library(duckdb)
library(tibble)
library(PatientProfiles)
cohort1 <- tibble::tibble(
  subject_id = c("1", "2", "3"),
  cohort_start_date = c(
    as.Date("2010-01-01"), as.Date("2010-01-01"), as.Date("2010-01-01")
  ),
  cohort_end_date = c(
    as.Date("2015-01-01"), as.Date("2013-01-01"), as.Date("2018-01-01")
  )
)
person <- tibble::tibble(
  person_id = c("1", "2", "3"),
  gender_concept_id = c("8507", "8532", "8507"),
  year_of_birth = c(1980, 1990, 2000),
  month_of_birth = c(NA, 07, 08),
  day_of_birth = c(01, 25, 03)
)
cdm <- mockCohortProfiles(person = person, cohort1 = cohort1)

## ---- message= FALSE, warning=FALSE-------------------------------------------
addAge(x = cdm$cohort1, cdm = cdm, defaultMonth = 12, defaultDay = 3)

## ---- message= FALSE, warning=FALSE-------------------------------------------
addAgeGroup(x = cdm$cohort1, cdm = cdm, ageGroup = list(c(20, 30), c(1, 19)))

