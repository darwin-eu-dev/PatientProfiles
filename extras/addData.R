library(readr)
library(here)
library(usethis)
library(stringr)
library(dplyr)

formats <- read_csv(here("extras", "formats.csv"), col_types = "c")

variables <- list(
  "date" = c("mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing"),
  "numeric" = c("mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing"),
  "integer" = c("mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing"),
  "binary" = c("count", "percentage"),
  "categorical" = c("count", "percentage")
)

formats <- variables |>
  lapply(as_tibble) |>
  bind_rows(.id = "variable_type") |>
  rename("estimate_name" = "value") |>
  inner_join(formats, by = "estimate_name") |>
  mutate(estimate_type = if_else(estimate_type == "same", variable_type, estimate_type))

namesTable <- read_csv(
  here("extras", "namesTable.csv"),
  col_types = list(
    table_name = "c",
    start_date_name = "c",
    end_date_name = "c",
    concept_id_name = "c",
    source_concept_id_name = "c"
  )
)

use_data(formats, namesTable, internal = TRUE, overwrite = TRUE)

