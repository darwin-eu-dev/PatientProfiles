library(testthat)
library(PatientProfiles)

connectionDetails <- list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  scratch_schema = "main",
  write_schema = "main"
)
test_check("PatientProfiles")
disconnectMock(connectionDetails)
