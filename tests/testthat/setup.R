on_cran <- function() {
  !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}
if (!on_cran()) {
  withr::local_envvar(
    R_USER_CACHE_DIR = tempfile(),
    .local_envir = testthat::teardown_env(),
    EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
  )
  CDMConnector::downloadEunomiaData(overwrite = TRUE)
}
writeSchema <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(dbToTest,
    "duckdb" = c(schema = "main", prefix = "test_"),
    "sql server" = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
    "redshift" = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA")
  )
}
connection <- function(dbToTest = Sys.getenv("DB_TO_TEST", "duckdb")) {
  switch(dbToTest,
    "duckdb" = DBI::dbConnect(duckdb::duckdb(), ":memory:"),
    "sql server" = DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 18 for SQL Server",
      Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
      Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
      UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
      PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
      TrustServerCertificate = "yes",
      Port = 1433
    ),
    "redshift" = DBI::dbConnect(
      RPostgres::Redshift(),
      dbname = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
      port = Sys.getenv("CDM5_REDSHIFT_PORT"),
      host = Sys.getenv("CDM5_REDSHIFT_HOST"),
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    )
  )
}
readTables <- function(cdm) {
  con <- CDMConnector::cdmCon(cdm)
  CDMConnector::listTables(con = con) |> sort()
}
