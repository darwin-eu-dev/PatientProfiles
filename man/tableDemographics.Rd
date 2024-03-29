% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableSummarised.R
\name{tableDemographics}
\alias{tableDemographics}
\title{Format a summariseDemographics result into a visual table.}
\usage{
tableDemographics(
  result,
  type = "gt",
  formatEstimateName = c(`N (\%)` = "<count> (<percentage>\%)", `Median [Q25 - Q75]` =
    "<median> [<q25> - <q75>]", `[Q05 - Q95]` = "[<q05> - <q95>]", `Mean (SD)` =
    "<mean> (<sd>)", Range = "<min> to <max>"),
  header = c("group"),
  split = c("group", "strata"),
  groupColumn = NULL,
  minCellCount = 5,
  excludeColumns = c("result_id", "result_type", "package_name", "package_version",
    "estimate_type", "additional_name", "additional_level"),
  .options = list()
)
}
\arguments{
\item{result}{A result from summariseDemographics.}

\item{type}{Type of desired formatted table, possibilities: "gt",
"flextable", "tibble".}

\item{formatEstimateName}{Named list of estimate name's to join, sorted by
computation order. Indicate estimate_name's between <...>.}

\item{header}{A vector containing which elements should go into the header
in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
`variable`, `estimate`, `settings`.}

\item{split}{A vector containing the name-level groups to split ("group",
"strata", "additional"), or an empty character vector to not split.}

\item{groupColumn}{Column to use as group labels.}

\item{minCellCount}{Counts below which results will be clouded.}

\item{excludeColumns}{Columns to drop from the output table.}

\item{.options}{Named list with additional formatting options.
PatientProfiles::optionsTableCharacteristics() shows allowed arguments and
their default values.}
}
\value{
A table with a formatted version of a summariseDemographics result.
}
\description{
`r lifecycle::badge("experimental")`
}
\examples{
\donttest{
library(PatientProfiles)

cdm <- mockPatientProfiles()

cdm$cohort1 |>
 summariseDemographics() |>
 tableDemographics()

CDMConnector::cdmDisconnect(cdm = cdm)
}

}
