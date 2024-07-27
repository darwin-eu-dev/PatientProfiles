library(readr)
library(here)
library(usethis)
library(stringr)
library(dplyr)

formats <- read_csv(here("extras", "formats.csv"), col_types = "c")

variables <- list(
  "date" = c("mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing"),
  "numeric" = c("sum", "mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing", "count", "percentage"),
  "integer" = c("sum", "mean", "sd", "median", "qXX", "min", "max", "count_missing", "percentage_missing", "count", "percentage"),
  "categorical" = c("count", "percentage"),
  "logical" = c("count", "percentage")
)

formats <- variables |>
  lapply(as_tibble) |>
  bind_rows(.id = "variable_type") |>
  rename("estimate_name" = "value") |>
  inner_join(formats, by = "estimate_name") |>
  mutate(estimate_type = if_else(estimate_type == "same", variable_type, estimate_type)) |>
  mutate(estimate_type = if_else(estimate_name %in% c("mean", "sd") & variable_type == "integer", "numeric", estimate_type)) |>
  mutate(estimate_description = case_when(
    variable_type %in% c("numeric", "integer") & estimate_name == "count" ~
      "count number of `1`.",
    variable_type %in% c("numeric", "integer") & estimate_name == "percentage" ~
      "percentage of occurrences of `1` (NA are excluded).",
    variable_type == "logical" & estimate_name == "count" ~
      "count number of `TRUE`.",
    variable_type == "logical" & estimate_name == "percentage" ~
      "percentage of occurrences of `TRUE` (NA are excluded).",
    .default = estimate_description
  ))

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

formatsOld <- read_csv(here("extras", "formats_old.csv"), col_types = "c")

estimatesFunc <- c(
  "min" = "base::min(x, na.rm = TRUE)",
  "max" = "base::max(x, na.rm = TRUE)",
  "mean" = "base::mean(x, na.rm = TRUE)",
  "median" = "stats::median(x, na.rm = TRUE)",
  "sum" = "base::sum(x, na.rm = TRUE)",
  "sd" = "stats::sd(x, na.rm = TRUE)")
quantiles <- paste0("stats::quantile(x, ", seq(0.01, 0.99, 0.01), ", na.rm = TRUE)")
names(quantiles) <- c(paste0("q0", 1:9), paste0("q", 10:99))
estimatesFunc <- c(estimatesFunc, quantiles)

use_data(estimatesFunc, formats, namesTable, formatsOld, internal = TRUE, overwrite = TRUE)

