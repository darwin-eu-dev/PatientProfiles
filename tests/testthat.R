library(testthat)
library(PatientProfiles)

duckdbConnectionDetails <- list(
  db = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  scratch_schema = "main",
  write_schema = "main"
)
test_check("PatientProfiles")
DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)



