
daysDiffQuery <- function(cdm,
                          start,
                          end,
                          names){

db <- omopgenerics::cdmSourceType(cdm)
q <- switch (
  db,
  "redshift" = glue::glue("DATEDIFF(day, {start}, {end})"),
  "postgresql" = glue::glue("(CAST({end} AS DATE) - CAST({start} AS DATE))"),
  "sql server" = glue::glue("DATEDIFF(day, {start}, {end})"),
  "duckdb" = glue::glue("datediff('day', {start}, {end})"),
  "snowflake" = glue::glue('DATEDIFF(day, "{start}", "{end}")'),
  rlang::abort(glue::glue("Connection type {db} is not supported!"))
)

q %>%
  rlang::parse_exprs() %>%
  rlang::set_names(glue::glue({names}))

}
