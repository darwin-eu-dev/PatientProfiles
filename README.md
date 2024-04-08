
# PatientProfiles <img src="man/figures/logo.png" align="right" height="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/PatientProfiles/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/PatientProfiles/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

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
#> Columns: 6
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ person_id                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ condition_concept_id      <int> 4, 3, 5, 2, 3, 4, 4, 3, 5, 4, 1, 1, 4, 4, 3,…
#> $ condition_start_date      <date> 2005-06-30, 2005-05-28, 2008-06-30, 2011-01…
#> $ condition_end_date        <date> 2007-07-25, 2007-09-16, 2010-10-06, 2011-10…
#> $ condition_type_concept_id <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date") %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ person_id                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ condition_concept_id      <int> 4, 3, 5, 2, 3, 4, 4, 3, 5, 4, 1, 1, 4, 4, 1,…
#> $ condition_start_date      <date> 2005-06-30, 2005-05-28, 2008-06-30, 2011-01…
#> $ condition_end_date        <date> 2007-07-25, 2007-09-16, 2010-10-06, 2011-10…
#> $ condition_type_concept_id <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ age                       <dbl> 7, 59, 9, 62, 69, 38, 23, 83, 43, 28, 47, 54…
#> $ sex                       <chr> "Female", "Female", "Male", "Female", "Male"…
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [?? x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>    condition_occurrence_id person_id condition_concept_id condition_start_date
#>                      <int>     <int>                <int> <date>              
#>  1                       6         6                    4 2005-09-23          
#>  2                       7         7                    4 2016-01-16          
#>  3                       9         9                    5 2009-12-21          
#>  4                      10        10                    4 2015-12-15          
#>  5                      11        11                    1 2014-04-08          
#>  6                      20        20                    4 2007-05-19          
#>  7                      24        24                    3 2005-02-16          
#>  8                      27        27                    2 2013-06-21          
#>  9                      46        46                    1 2007-10-10          
#> 10                      48        48                    2 2014-01-06          
#> # ℹ more rows
#> # ℹ 4 more variables: condition_end_date <date>,
#> #   condition_type_concept_id <dbl>, age <dbl>, sex <chr>
```

#### Adding characteristics of a cohort

As with other tables in the OMOP CDM, we can work in a similar way with
cohort tables. For example, say we have the below cohort table

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1, 2, 1
#> $ subject_id           <dbl> 1, 2, 3, 1
#> $ cohort_start_date    <date> 2020-06-01, 2020-01-02, 2020-01-01, 2020-01-01
#> $ cohort_end_date      <date> 2020-08-01, 2020-02-02, 2020-03-01, 2020-04-01
#> $ age                  <dbl> 22, 73, 21, 22
#> $ age_group            <chr> "19 to 65", "66 to 100", "19 to 65", "19 to 65"
#> $ sex                  <chr> "Female", "Female", "Male", "Female"
#> $ prior_observation    <dbl> 4209, 4486, 5267, 4057
```

We could use this information to subset the cohort. For example limiting
to those with at least 365 days of prior observation available before
their cohort start date like so

``` r
cdm$cohort1 %>%
  filter(prior_observation >= 365)
#> # Source:   SQL [4 x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>                  <dbl>      <dbl> <date>            <date>          <dbl>
#> 1                    1          1 2020-06-01        2020-08-01         22
#> 2                    1          2 2020-01-02        2020-02-02         73
#> 3                    2          3 2020-01-01        2020-03-01         21
#> 4                    1          1 2020-01-01        2020-04-01         22
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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    window = c(-Inf, -1)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_minf_to_m1  <dbl> 1, 1
#> $ cohort_2_minf_to_m1  <dbl> 0, 1
```

#### Count appearances of a certain cohort in a certain window

If we wanted the number of appearances, we could instead use the
`addCohortIntersectCount` function

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_mid_term    <dbl> 1, 0
#> $ cohort_1_short_term  <dbl> 0, 0
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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_minf_to_inf <date> 2019-12-30, 2019-12-30
```

Last occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_minf_to_inf <date> 2020-05-25, 2020-05-25
```

#### Add the number of days instead of the date

Instead of returning a date, we could return the days to the
intersection by using `addCohortIntersectDays`

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_minf_to_inf <dbl> 145, -7
```

#### Combine multiple cohort intersects

If we want to combine multiple cohort intersects we can concatenate the
operations using the `pipe` operator:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01
#> $ cohort_1_minf_to_inf <dbl> 2, 2
```

A more efficient implementation for getting multiple types of
intersection results is provided by `addCohortIntersectTime`

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <dbl> 1, 1
#> $ subject_id           <dbl> 1, 1
#> $ cohort_start_date    <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date      <date> 2020-04-01, 2020-08-01

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
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id    <dbl> 1, 1
#> $ subject_id              <dbl> 1, 1
#> $ cohort_start_date       <date> 2020-01-01, 2020-06-01
#> $ cohort_end_date         <date> 2020-04-01, 2020-08-01
#> $ count_cohort_1_any_time <dbl> 2, 2
#> $ flag_cohort_1_any_time  <dbl> 1, 1
#> $ date_cohort_1_any_time  <date> 2019-12-30, 2019-12-30
#> $ days_cohort_1_any_time  <dbl> -2, -154
```
