library(testthat)
library(PatientProfiles)

availableConnections <- list(
  duckdb <- list(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    scratch_schema = "main",
    write_schema = "main"
  )
)
for (k in seq_along(availableConnections)) {
  connectionDetails <- availableConnections[[k]]
  test_check("PatientProfiles")
  PatientProfiles:::disconnectMockCdm(connectionDetails)
}

