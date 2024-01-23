library(DBI)
library(CDMConnector)
library(dplyr)

server_dbi <- "cdm_gold_202207"
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdm <- cdmFromCon(
  con = db, cdmSchema = "public_100k",
  writeSchema = c(schema = "results", prefix = "lcwp1_"), cohortTables = "index"
)

window <- list(
  c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365),
  c(366, Inf)
)

devtools::load_all()

cdm$index_sex <- cdm$index %>% addSex()

tictoc::tic()
lsc <- summariseLargeScaleCharacteristics(cohort = cdm$index, window = window)
tictoc::toc()

tictoc::tic()
lsc <- summariseLargeScaleCharacteristics(cohort = cdm$index_sex, strata = list("sex"), window = window)
tictoc::toc()

tictoc::tic()
lsc <- DrugUtilisation::summariseLargeScaleCharacteristics(cohort = cdm$index, window = window, tablesToCharacterize = c("condition_occurrence", "drug_exposure"))
tictoc::toc()

tictoc::tic()
lsc <- DrugUtilisation::summariseLargeScaleCharacteristics(cohort = cdm$index_sex, strata = list("sex"), window = window, tablesToCharacterize = c("condition_occurrence", "drug_exposure"))
tictoc::toc()
