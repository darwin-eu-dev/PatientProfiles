
# PatientProfiles <img src='man/figures/Hex.png' align="right" height="139"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/PatientProfiles/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/PatientProfiles/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Package overview

PatientProfiles contains functions for adding characteristics to OMOP
CDM tables containing patient level data (e.g. condition occurrence,
drug exposure, and so on) and OMOP CDM cohort tables. The
characteristics that can be added include an individual´s sex, age, and
days of prior observation Time varying characteristics, such as age, can
be estimated relative to any date in the corresponding table. In
addition, PatientProfiles also provides functionality for identifying
intersections between a cohort table and OMOP CDM tables containing
patient level data or other cohort tables.

## Package installation

You can install the latest version of PatientProfiles like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/PatientProfiles")
```

## Example usage

### Create a reference to data in the OMOP CDM format

The PatientProfiles package is designed to work with data in the OMOP
CDM format, so our first step is to create a reference to the data using
the CDMConnector package.

``` r
library(CDMConnector)
library(PatientProfiles)
library(dplyr)
```

Creating a connection to a Postgres database would for example look
like:

``` r
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
  host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
)

cdm <- cdm_from_con(
  con,
  cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
  write_schema = Sys.getenv("CDM5_POSTGRESQL_RESULT_SCHEMA")
)
```

To see how you would create a reference to your database please consult
the CDMConnector package
[documentation](https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html).
For this example though we’ll work with simulated data, and we’ll
generate an example cdm reference like so:

``` r
cdm <- mockPatientProfiles(patient_size = 1000, drug_exposure_size = 1000)
```

### Adding individuals´ characteristics

#### Adding characteristics to patient-level data

Say we wanted to get individuals´sex and age at condition start date for
records in the condition occurrence table. We can use the `addAge` and
`addSex` functions to do this:

``` r
cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ condition_occurrence_id <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ person_id               <int> 314, 970, 349, 234, 193, 919, 113, 277, 491, 9…
#> $ condition_concept_id    <int> 4, 5, 1, 2, 3, 4, 3, 2, 1, 2, 1, 5, 3, 4, 4, 1…
#> $ condition_start_date    <date> 2005-08-25, 2007-02-15, 2009-02-15, 2008-05-0…
#> $ condition_end_date      <date> 2006-06-14, 2007-06-05, 2011-06-24, 2009-01-0…

cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date") %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ condition_occurrence_id <int> 314, 234, 277, 26, 276, 656, 275, 249, 150, 9,…
#> $ person_id               <int> 314, 234, 277, 26, 276, 656, 275, 249, 150, 9,…
#> $ condition_concept_id    <int> 4, 2, 2, 1, 5, 3, 4, 2, 4, 5, 2, 2, 2, 3, 1, 1…
#> $ condition_start_date    <date> 2005-08-25, 2008-05-03, 2007-04-26, 2009-01-0…
#> $ condition_end_date      <date> 2006-06-14, 2009-01-05, 2007-05-07, 2010-02-0…
#> $ age                     <dbl> 58, 57, 22, 9, 76, 89, 70, 49, 55, 17, 39, 26,…
#> $ sex                     <chr> "Male", "Male", "Male", "Female", "Male", "Mal…
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [?? x 7]
#> # Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#>    condition_occurrence_id person_id condition_concept_id condition_start_date
#>                      <int>     <int>                <int> <date>              
#>  1                     314       314                    4 2005-08-25          
#>  2                     234       234                    2 2008-05-03          
#>  3                     277       277                    2 2007-04-26          
#>  4                     249       249                    2 2014-12-08          
#>  5                     150       150                    4 2011-08-31          
#>  6                     406       406                    3 2009-02-11          
#>  7                     438       438                    1 2008-09-26          
#>  8                     536       536                    4 2009-11-11          
#>  9                     872       872                    2 2006-10-19          
#> 10                     424       424                    4 2009-01-12          
#> # ℹ more rows
#> # ℹ 3 more variables: condition_end_date <date>, age <dbl>, sex <chr>
```

#### Adding characteristics of a cohort

As with other tables in the OMOP CDM, we can work in a similar way with
cohort tables. For example, say we have the below cohort table

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01
```

We can add age, age groups, sex, and days of prior observation to a
cohort like so

``` r
cdm$cohort1 <- cdm$cohort1 %>%
  addAge(
    indexDate = "cohort_start_date",
    ageGroup = list(c(0, 18), c(19, 65), c(66, 100))
  ) %>%
  addSex() %>%
  addPriorObservation()

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 2, 1, 1, 1
#> $ subject_id           <dbl> 3, 1, 2, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-03-01, 2020-08-01, 2020-02-02, 2020-04-01
#> $ age                  <dbl> 42, 41, 50, 40
#> $ age_group            <chr> "19 to 65", "19 to 65", "19 to 65", "19 to 65"
#> $ sex                  <chr> "Female", "Male", "Male", "Male"
#> $ prior_observation    <dbl> 4635, 5350, 4168, 5198
```

We could use this information to subset the cohort. For example limiting
to those with at least 365 days of prior observation available before
their cohort start date like so

``` r
cdm$cohort1 %>%
  filter(prior_observation >= 365)
#> # Source:   SQL [4 x 8]
#> # Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>                  <dbl>      <dbl> <date>            <date>          <dbl>
#> 1                    2          3 2020-01-01        2020-03-01         42
#> 2                    1          1 2020-06-01        2020-08-01         41
#> 3                    1          2 2020-01-02        2020-02-02         50
#> 4                    1          1 2020-01-01        2020-04-01         40
#> # ℹ 3 more variables: age_group <chr>, sex <chr>, prior_observation <dbl>
```

### Cohort intersections

#### Detect the presence of another cohort in a certain window

We can use `addCohortIntersectFlag` to add a flag for the presence (or
not) of a cohort in a certain window.

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    window = c(-Inf, -1)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01
#> $ cohort_1_minf_to_m1  <dbl> 1, 1, 0, 0
#> $ cohort_2_minf_to_m1  <dbl> 0, 1, 0, 0
#> $ cohort_3_minf_to_m1  <dbl> 0, 0, 0, 0
```

#### Count appearances of a certain cohort in a certain window

If we wanted the number of appearances, we could instead use the
`addCohortIntersectCount` function

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = list("short_term" = c(1, 30), "mid_term" = c(31, 180))
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 1, 3, 2
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ cohort_1_short_term  <dbl> 0, 0, 0, 0
#> $ cohort_1_mid_term    <dbl> 1, 0, 0, 0
```

#### Add a column with the first/last event in a certain window

Say we wanted the date at which an individual was in another cohort then
we can use the `addCohortIntersectDate` function. As there might be
multiple records for the other cohort, we can also choose the first or
the last appearance in that cohort.

First occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "first",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 1, 3, 2
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ cohort_1_minf_to_inf <date> 2019-12-30, 2019-12-30, NA, NA
```

Last occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 1, 3, 2
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ cohort_1_minf_to_inf <date> 2020-05-25, 2020-05-25, NA, NA
```

#### Add the number of days instead of the date

Instead of returning a date, we could return the days to the
intersection by using `addCohortIntersectDays`

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDays(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 1, 3, 2
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ cohort_1_minf_to_inf <dbl> 145, -7, NA, NA
```

#### Combine multiple cohort intersects

If we want to combine multiple cohort intersects we can concatenate the
operations using the `pipe` operator:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectDate(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    order = "last",
    window = c(-Inf, Inf)
  ) %>%
  addCohortIntersectCount(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    window = c(-Inf, Inf)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 5
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 1, 3, 2
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ cohort_1_minf_to_inf <dbl> 2, 2, NA, NA
```

A more efficient implementation for getting multiple types of
intersection results is provided by `addCohortIntersectTime`

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 1, 2
#> $ subject_id           <dbl> 1, 1, 2, 3
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01, 2020-01-02, 2020-01-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01, 2020-02-02, 2020-03-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersect(
    targetCohortTable = "cohort2",
    targetCohortId = 1,
    count = TRUE,
    flag = TRUE,
    days = TRUE,
    date = TRUE,
    window = list("any_time" = c(-Inf, Inf))
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB 0.7.1 [martics@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id    <dbl> 1, 1, 2, 1
#> $ subject_id              <dbl> 1, 1, 3, 2
#> $ cohort_start_date       <date> 2020-01-01, 2020-06-01, 2020-01-01, 2020-01-02
#> $ cohort_end_date         <date> 2020-04-01, 2020-08-01, 2020-03-01, 2020-02-02
#> $ count_cohort_1_any_time <dbl> 2, 2, 0, 0
#> $ flag_cohort_1_any_time  <dbl> 1, 1, 0, 0
#> $ date_cohort_1_any_time  <date> 2019-12-30, 2019-12-30, NA, NA
#> $ days_cohort_1_any_time  <dbl> -2, -154, NA, NA
```
