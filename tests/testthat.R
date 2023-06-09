library(testthat)
library(PatientProfiles)

availableConnections <- list(list(
  con = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  scratch_schema = "main",
  write_schema = "main"
))

# if (Sys.getenv("CDM5_REDSHIFT_DBNAME") != "") {
#   availableConnections <- availableConnections %>%
#     append(value = list(list(
#       con = DBI::dbConnect(
#         RPostgres::Redshift(),
#         dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
#         port = Sys.getenv("CDM5_REDSHIFT_PORT"),
#         host = Sys.getenv("CDM5_REDSHIFT_HOST"),
#         user = Sys.getenv("CDM5_REDSHIFT_USER"),
#         password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
#       ),
#       scratch_schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
#       write_schema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
#     )))
# }

# if (Sys.getenv("CDM5_SQL_SERVER_USER") != "") {
#   availableConnections <- availableConnections %>%
#     append(value = list(list(
#       con = DBI::dbConnect(
#         odbc::odbc(),
#         Driver   = "ODBC Driver 18 for SQL Server",
#         Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
#         Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
#         UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
#         PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
#         TrustServerCertificate = "yes",
#         Port     = 1433
#       ),
#       scratch_schema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
#       write_schema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
#     )))
# }

availableConnections <- list(list(
  con = DBI::dbConnect(
    odbc::odbc(),
    Driver   = "ODBC Driver 18 for SQL Server",
    Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
    Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
    UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
    PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
    TrustServerCertificate = "yes",
    Port     = 1433
  ),
  scratch_schema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
  write_schema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
))


for (k in seq_along(availableConnections)) {
  connectionDetails <- availableConnections[[k]]
  test_check("PatientProfiles")
  PatientProfiles:::disconnectMockCdm(connectionDetails)
}
