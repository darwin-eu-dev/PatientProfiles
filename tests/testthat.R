library(testthat)
library(PatientProfiles)

print(Sys.getenv("CDM5_REDSHIFT_DBNAME"))

availableConnections <- list(
  duckdb <- list(
    con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    scratch_schema = "main",
    write_schema = "main"
  ),
  redshift <- list(
    con = DBI::dbConnect(
      RPostgres::Redshift(), dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
      port = Sys.getenv("CDM5_REDSHIFT_PORT"),
      host = Sys.getenv("CDM5_REDSHIFT_HOST"),
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    ),
    scratch_schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
    write_schema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
  )
)
for (k in seq_along(availableConnections)) {
  connectionDetails <- availableConnections[[k]]
  test_check("PatientProfiles")
  PatientProfiles:::disconnectMockCdm(connectionDetails)
}

