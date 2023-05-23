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

allFunctions <- c(
  unique(formats$format_key),
  paste0("q", str_pad(seq(0, 100, 5), 2, pad = "0"))
)
allFunctions <- allFunctions[allFunctions != "qXX"]

use_data(formats, allFunctions, internal = TRUE, overwrite = TRUE)
