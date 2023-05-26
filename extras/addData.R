library(readr)
library(here)
library(usethis)
library(stringr)

formats <- read_csv(
  here("extras", "formats.csv"),
  col_types = list(
    variable_classification = "c",
    format_key = "c",
    applied_function = "c",
    result = "c"
  )
)

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

