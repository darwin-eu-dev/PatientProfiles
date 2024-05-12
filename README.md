
# PatientProfiles <img src="man/figures/logo.png" align="right" height="200"/>

[![CRAN
status](https://www.r-pkg.org/badges/version/PatientProfiles)](https://CRAN.R-project.org/package=PatientProfiles)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/PatientProfiles/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu-dev/PatientProfiles?branch=main)
[![R-CMD-check](https://github.com/darwin-eu-dev/PatientProfiles/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/PatientProfiles/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/PatientProfiles)](https://cran.r-project.org/package=PatientProfiles)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PatientProfiles)](https://cran.r-project.org/package=PatientProfiles)

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
install.packages("PatientProfiles")
```

## Citation

``` r
citation("PatientProfiles")
#> 
#> To cite package 'PatientProfiles' in publications use:
#> 
#>   Catala M, Guo Y, Du M, Lopez-Guell K, Burn E (????).
#>   _PatientProfiles: Identify Characteristics of Patients in the OMOP
#>   Common Data Model_. R package version 1.0.0,
#>   <https://darwin-eu-dev.github.io/PatientProfiles/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {PatientProfiles: Identify Characteristics of Patients in the OMOP Common Data Model},
#>     author = {Marti Catala and Yuchen Guo and Mike Du and Kim Lopez-Guell and Edward Burn},
#>     note = {R package version 1.0.0},
#>     url = {https://darwin-eu-dev.github.io/PatientProfiles/},
#>   }
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
cdm <- mockPatientProfiles(numberIndividuals = 1000)
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
#> $ person_id                 <int> 343, 221, 690, 667, 366, 131, 929, 489, 196,…
#> $ condition_start_date      <date> 2048-10-14, 1992-09-30, 2003-10-15, 1927-04…
#> $ condition_end_date        <date> 2055-04-14, 2090-10-09, 2023-06-10, 1954-02…
#> $ condition_occurrence_id   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ condition_concept_id      <int> 10, 8, 3, 4, 4, 4, 10, 2, 8, 8, 10, 2, 6, 5,…
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

cdm$condition_occurrence <- cdm$condition_occurrence %>%
  addAge(indexDate = "condition_start_date") %>%
  addSex()

cdm$condition_occurrence %>%
  glimpse()
#> Rows: ??
#> Columns: 8
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ person_id                 <int> 343, 221, 667, 366, 131, 929, 489, 196, 496,…
#> $ condition_start_date      <date> 2048-10-14, 1992-09-30, 1927-04-02, 2107-05…
#> $ condition_end_date        <date> 2055-04-14, 2090-10-09, 1954-02-01, 2135-04…
#> $ condition_occurrence_id   <int> 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, …
#> $ condition_concept_id      <int> 10, 8, 4, 4, 4, 10, 2, 8, 8, 10, 2, 6, 5, 2,…
#> $ condition_type_concept_id <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ age                       <int> 46, 15, 7, 98, 11, 42, 33, 80, 15, 33, 82, 3…
#> $ sex                       <chr> "Female", "Male", "Female", "Female", "Femal…
```

We could, for example, then limit our data to only males aged between 18
and 65

``` r
cdm$condition_occurrence %>%
  filter(age >= 18 & age <= 65) %>%
  filter(sex == "Male")
#> # Source:   SQL [?? x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>    person_id condition_start_date condition_end_date condition_occurrence_id
#>        <int> <date>               <date>                               <int>
#>  1       608 2000-09-08           2081-12-17                              13
#>  2       307 1980-02-14           2017-10-12                              15
#>  3       698 2022-09-13           2083-03-02                              19
#>  4       338 2025-12-14           2028-05-03                              30
#>  5       864 1964-11-09           2029-01-22                              38
#>  6        20 2020-09-21           2108-07-09                              65
#>  7       470 1961-08-02           1966-12-28                              68
#>  8       469 1984-03-16           2006-05-18                              70
#>  9       821 1954-08-11           1962-01-30                              79
#> 10       765 1934-02-08           1956-05-07                              80
#> # ℹ more rows
#> # ℹ 4 more variables: condition_concept_id <int>,
#> #   condition_type_concept_id <int>, age <int>, sex <chr>
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
#> $ cohort_definition_id <int> 1, 2, 3, 3, 3, 3, 1, 3, 1, 1, 3, 3, 2, 3, 1, 3, 2…
#> $ subject_id           <int> 260, 244, 759, 439, 113, 776, 327, 985, 707, 297,…
#> $ cohort_start_date    <date> 2069-08-23, 1990-11-12, 2011-05-27, 1976-07-18, …
#> $ cohort_end_date      <date> 2097-12-24, 2023-09-10, 2016-01-30, 2023-06-16, …
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
#> $ cohort_definition_id <int> 1, 2, 3, 3, 3, 1, 1, 3, 2, 3, 1, 3, 2, 1, 3, 3, 1…
#> $ subject_id           <int> 260, 244, 439, 113, 776, 327, 297, 27, 592, 378, …
#> $ cohort_start_date    <date> 2069-08-23, 1990-11-12, 1976-07-18, 2055-08-21, …
#> $ cohort_end_date      <date> 2097-12-24, 2023-09-10, 2023-06-16, 2077-06-10, …
#> $ age                  <int> 63, 62, 3, 132, 24, 20, 65, 67, 70, 3, 122, 55, 2…
#> $ age_group            <chr> "19 to 65", "19 to 65", "0 to 18", "None", "19 to…
#> $ sex                  <chr> "Female", "Male", "Male", "Male", "Male", "Female…
#> $ prior_observation    <int> 23245, 22961, 1294, 48445, 8844, 7562, 24057, 246…
```

We could use this information to subset the cohort. For example limiting
to those with at least 365 days of prior observation available before
their cohort start date like so

``` r
cdm$cohort1 %>%
  filter(prior_observation >= 365)
#> # Source:   SQL [?? x 8]
#> # Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#>    cohort_definition_id subject_id cohort_start_date cohort_end_date   age
#>                   <int>      <int> <date>            <date>          <int>
#>  1                    1        260 2069-08-23        2097-12-24         63
#>  2                    2        244 1990-11-12        2023-09-10         62
#>  3                    3        439 1976-07-18        2023-06-16          3
#>  4                    3        113 2055-08-21        2077-06-10        132
#>  5                    3        776 1989-03-20        2013-04-06         24
#>  6                    1        327 1989-09-15        2088-07-26         20
#>  7                    1        297 2017-11-12        2080-11-05         65
#>  8                    3         27 2033-07-08        2069-03-05         67
#>  9                    2        592 2045-05-28        2063-12-28         70
#> 10                    3        378 1936-09-05        2084-11-29          3
#> # ℹ more rows
#> # ℹ 3 more variables: age_group <chr>, sex <chr>, prior_observation <int>
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
#> $ cohort_definition_id <int> 3, 3, 3, 2, 3, 2, 1, 1, 2, 1
#> $ subject_id           <int> 5, 3, 1, 6, 7, 10, 2, 8, 9, 4
#> $ cohort_start_date    <date> 2106-03-21, 2078-07-16, 2107-03-02, 2036-12-07, 2…
#> $ cohort_end_date      <date> 2112-08-12, 2151-05-18, 2110-05-11, 2040-01-08, 2…

cdm$cohort1 <- cdm$cohort1 %>%
  addCohortIntersectFlag(
    targetCohortTable = "cohort2",
    window = c(-Inf, -1)
  )

cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 3, 3, 3, 2, 3, 2, 1, 1, 2, 1
#> $ subject_id           <int> 5, 3, 1, 6, 7, 10, 2, 8, 9, 4
#> $ cohort_start_date    <date> 2106-03-21, 2078-07-16, 2107-03-02, 2036-12-07, 2…
#> $ cohort_end_date      <date> 2112-08-12, 2151-05-18, 2110-05-11, 2040-01-08, 2…
#> $ cohort_1_minf_to_m1  <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_2_minf_to_m1  <dbl> 1, 1, 0, 1, 0, 1, 0, 0, 0, 0
#> $ cohort_3_minf_to_m1  <dbl> 0, 0, 0, 0, 1, 0, 1, 1, 1, 0
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
#> $ cohort_definition_id <int> 3, 3, 2, 2, 3, 3, 3, 3, 1, 2
#> $ subject_id           <int> 10, 5, 1, 6, 7, 3, 4, 2, 9, 8
#> $ cohort_start_date    <date> 1979-02-05, 1945-07-26, 1964-06-02, 2025-10-20, 2…
#> $ cohort_end_date      <date> 1999-05-30, 2009-10-12, 1968-10-19, 2105-06-01, 2…

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
#> $ cohort_definition_id <int> 2, 3, 3, 2, 3, 3, 3, 3, 1, 2
#> $ subject_id           <int> 6, 10, 5, 1, 7, 3, 4, 2, 9, 8
#> $ cohort_start_date    <date> 2025-10-20, 1979-02-05, 1945-07-26, 1964-06-02, 2…
#> $ cohort_end_date      <date> 2105-06-01, 1999-05-30, 2009-10-12, 1968-10-19, 2…
#> $ cohort_1_mid_term    <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
#> $ cohort_1_short_term  <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
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
#> $ cohort_definition_id <int> 3, 2, 3, 3, 1, 3, 2, 3, 1, 1
#> $ subject_id           <int> 2, 4, 6, 9, 3, 10, 1, 5, 7, 8
#> $ cohort_start_date    <date> 2005-11-08, 1999-07-08, 2069-10-11, 1999-10-16, 1…
#> $ cohort_end_date      <date> 2012-08-06, 2047-07-13, 2074-12-30, 2104-04-26, 2…

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
#> $ cohort_definition_id <int> 3, 3, 3, 2, 3, 3, 1, 2, 1, 1
#> $ subject_id           <int> 2, 10, 5, 4, 6, 9, 3, 1, 7, 8
#> $ cohort_start_date    <date> 2005-11-08, 1966-07-15, 2046-07-28, 1999-07-08, 2…
#> $ cohort_end_date      <date> 2012-08-06, 1996-04-01, 2065-12-21, 2047-07-13, 2…
#> $ cohort_1_minf_to_inf <date> 1922-03-06, 1959-03-23, 2039-10-15, NA, NA, NA, …
```

Last occurrence:

``` r
cdm$cohort1 %>%
  glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB v0.10.0 [martics@Windows 10 x64:R 4.2.1/:memory:]
#> $ cohort_definition_id <int> 1, 1, 3, 3, 2, 1, 1, 3, 1, 3
#> $ subject_id           <int> 2, 10, 5, 1, 7, 3, 4, 9, 8, 6
#> $ cohort_start_date    <date> 1930-01-06, 1940-10-23, 1916-02-17, 2066-03-17, 1…
#> $ cohort_end_date      <date> 2025-06-17, 1968-04-18, 1919-01-10, 2066-10-13, 1…

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
#> $ cohort_definition_id <int> 1, 1, 2, 1, 3, 3, 3, 1, 1, 3
#> $ subject_id           <int> 2, 10, 7, 4, 9, 5, 1, 3, 8, 6
#> $ cohort_start_date    <date> 1930-01-06, 1940-10-23, 1958-11-08, 2077-04-26, 2…
#> $ cohort_end_date      <date> 2025-06-17, 1968-04-18, 1959-08-14, 2077-05-10, 2…
#> $ cohort_1_minf_to_inf <date> 1938-01-23, 1968-03-03, 1942-06-19, 2045-06-16, …
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
#> $ cohort_definition_id <int> 2, 3, 3, 2, 2, 3, 3, 3, 3, 2
#> $ subject_id           <int> 7, 4, 1, 9, 10, 5, 3, 8, 6, 2
#> $ cohort_start_date    <date> 2053-01-09, 1961-07-08, 2082-07-12, 1982-02-02, 1…
#> $ cohort_end_date      <date> 2118-03-01, 2103-08-06, 2096-08-04, 1989-05-29, 1…

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
#> $ cohort_definition_id <int> 3, 2, 3, 3, 2, 2, 3, 3, 3, 2
#> $ subject_id           <int> 3, 7, 4, 1, 9, 10, 5, 8, 6, 2
#> $ cohort_start_date    <date> 2058-05-14, 2053-01-09, 1961-07-08, 2082-07-12, 1…
#> $ cohort_end_date      <date> 2158-06-30, 2118-03-01, 2103-08-06, 2096-08-04, 1…
#> $ cohort_1_minf_to_inf <dbl> -13451, NA, NA, NA, NA, NA, NA, NA, NA, NA
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
#> $ cohort_definition_id <int> 3, 3, 1, 2, 3, 1, 2, 1, 3, 3
#> $ subject_id           <int> 7, 6, 9, 3, 1, 2, 8, 5, 4, 10
#> $ cohort_start_date    <date> 2017-03-21, 1961-10-26, 2078-03-01, 2000-12-20, 2…
#> $ cohort_end_date      <date> 2137-02-07, 2068-12-02, 2088-09-29, 2046-05-16, 2…

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
#> $ cohort_definition_id <int> 3, 1, 3, 3, 2, 1, 2, 1, 3, 3
#> $ subject_id           <int> 6, 9, 1, 7, 3, 2, 8, 5, 4, 10
#> $ cohort_start_date    <date> 1961-10-26, 2078-03-01, 2000-10-31, 2017-03-21, 2…
#> $ cohort_end_date      <date> 2068-12-02, 2088-09-29, 2063-05-21, 2137-02-07, 2…
#> $ cohort_1_minf_to_inf <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0
```

``` r
mockDisconnect(cdm)
```
