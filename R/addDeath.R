# Copyright 2024 DARWIN EU (C)
#
# This file is part of PatientProfiles
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Add date of death for individuals. Only death within the same observation
#' period than `indexDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathDateName name of the new column to be added.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathDate()
#' mockDisconnect(cdm = cdm)
#' }
#'
addDeathDate <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathDateName = "date_of_death",
                         name = NULL) {
  addDeath(
    x = x,
    value = "date",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathDateName,
    name = name
  )
}

#' Add days to death for individuals. Only death within the same observation
#' period than `indexDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathDaysName name of the new column to be added.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathDays()
#' mockDisconnect(cdm = cdm)
#' }
#'
addDeathDays <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathDaysName = "days_to_death",
                         name = NULL) {
  addDeath(
    x = x,
    value = "days",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathDaysName,
    name = name
  )
}


#' Add flag for death for individuals. Only death within the same observation
#' period than `indexDate` will be observed.
#'
#' @param x Table with individuals in the cdm.
#' @param indexDate Variable in x that contains the window origin.
#' @param censorDate Name of a column to stop followup.
#' @param window window to consider events over.
#' @param deathFlagName name of the new column to be added.
#' @param name Name of the new table, if NULL a temporary table is returned.
#'
#' @return table x with the added column with death information added.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPatientProfiles()
#' cdm$cohort1 %>%
#'   addDeathFlag()
#' mockDisconnect(cdm = cdm)
#' }
#'
addDeathFlag <- function(x,
                         indexDate = "cohort_start_date",
                         censorDate = NULL,
                         window = c(0, Inf),
                         deathFlagName = "death",
                         name = NULL) {
  addDeath(
    x = x,
    value = "flag",
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    deathName = deathFlagName,
    name = name
  )
}



addDeath <- function(x,
                     value,
                     indexDate,
                     censorDate,
                     window,
                     deathName,
                     name) {

  # input validation
  omopgenerics::assertTable(x, columns = c(indexDate))
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertTable(cdm[["death"]])
  window <- omopgenerics::validateWindowArgument(window)
  window <- purrr::list_c(window)
  deathName <- omopgenerics::validateNameArgument(deathName, validation = "warning")
  if (deathName %in% colnames(x)) {
    cli::cli_warn("{deathName} variable already exists and will be overwritten")
    x <- x |>
      dplyr::select(!dplyr::all_of(deathName))
  }

  x <- x |>
    .addIntersect(
      tableName = "death",
      value = value,
      indexDate = indexDate,
      censorDate = censorDate,
      window = window,
      targetStartDate = "death_date",
      targetEndDate = NULL,
      order = "first",
      nameStyle = deathName,
      name = name
    )

  return(x)
}
