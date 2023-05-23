library(readr)
library(here)
library(usethis)
library(stringr)

formats <- read_csv(
  here("extras", "formats.csv"),
  col_types = list(
    type = "c",
    format_key = "c",
    applied_function = "c"
  )
)

use_data(formats, internal = TRUE, overwrite = TRUE)
