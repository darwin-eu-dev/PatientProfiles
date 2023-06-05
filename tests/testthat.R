library(testthat)
library(PatientProfiles)

availableConnections <- list(
  duckdb <- list(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    scratch_schema = "main",
    write_schema = "main"
  )
)
for (k in seq_len(availableConnections)) {
  connectionDetails <- availableConnections[[k]]
  test_check("PatientProfiles")
  disconnectMockCdm(connectionDetails)
}

