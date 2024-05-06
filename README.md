
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthdb <a href="https://kevinhzq.github.io/healthdb/"><img src="man/figures/logo.png" alt="healthdb website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/KevinHzq/healthdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KevinHzq/healthdb/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KevinHzq/healthdb/branch/master/graph/badge.svg)](https://app.codecov.io/gh/KevinHzq/healthdb?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/healthdb)](https://CRAN.R-project.org/package=healthdb)

<!-- badges: end -->

The goal of ‘healthdb’ is to provide a set of tools for identifying
diseases or events from healthcare database and preparing data for
epidemiological studies. It features abilities that are not natively
support by database, such as matching strings by ‘stringr’ style regular
expression and using ‘LIKE’ operator with multiple patterns in a vector.
Three types of functions are included: interactive functions – for
customizing complex definitions; call building functions – for batch
execution of simple definition; miscellaneous functions – for data
wrangling, computing age and comorbidity index, etc.

**The package is tested only on SQL Server and SQLite** as we do not
have access to other SQL dialects. Please report bugs if you encounter
issues with other dialects.

Administrative health data are often stored on SQL database with strict
security measures which may disable permission to write temporary
tables. Writing queries without being able to cache intermediate results
is challenging, especially when the data is too large to be downloaded
from database into R (i.e., local memory) without some filtering
process.

This package leverages ‘dbplyr’, particularly its ability to chain
subqueries, in order to implement a common disease definition as a
one-shot big query. Outputs are fully compatible with ‘dplyr’ functions.

Common disease definitions often are in the form of having n primary
care/hospitalization/prescription records with some International
Classification of Diseases (ICD) codes within some time span. See below
for an example of implementing such case definition.

## Installation

Install from CRAN:

``` r
install.packages("healthdb")
```

You could also install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KevinHzq/healthdb")
```

## Usage

We are going to implement the following case definition:

One or more hospitalization with a substance use disorder (SUD) ICD-9
diagnostic code, OR Two or more physician claims with a substance use
disorder ICD-10 diagnostic code within one year.

Before we get started, please see [how to connect to a
database](https://solutions.posit.co/connections/db/getting-started/connect-to-database/)
and [how to write query with
‘dbplyr’](https://solutions.posit.co/connections/db/getting-started/database-queries/)
if you don’t have experience of working with database in R.

First, let’s make a demo data sets for the two sources:

Physician claims

``` r
library(healthdb)
library(tidyverse)

# make_test_dat() makes either a toy data.frame or database table in memory with known number of rows that satisfy the query we will show later
claim_db <- make_test_dat(vals_kept = c("303", "304", "305", "291", "292", str_glue("30{30:59}"), str_glue("29{10:29}"), noise_val = c("999", "111")), type = "database")

# this is a database table
# note that in-memory SQLite database stores dates as numbers
claim_db %>% head()
#> # Source:   SQL [6 x 6]
#> # Database: sqlite 3.45.2 [:memory:]
#>     uid clnt_id dates diagx diagx_1 diagx_2
#>   <int>   <int> <dbl> <chr> <chr>   <chr>  
#> 1    81       2 16986 999   <NA>    999    
#> 2    82       3 17018 999   999     <NA>   
#> 3    24       3 17352 291   3047    <NA>   
#> 4     4       3 17554 2913  3032    <NA>   
#> 5    28       4 17750 2929  2925    <NA>   
#> 6    74       4 18196 999   <NA>    <NA>
```

Hospitalization

``` r
hosp_df <- make_test_dat(vals_kept = c(str_glue("F{10:19}"), str_glue("F{100:199}"), noise_val = "999"), type = "data.frame")

# this is a local data.frame/tibble
hosp_df %>% head()
#>   uid clnt_id      dates diagx diagx_1 diagx_2
#> 1   8       1 2017-06-06   F14    F142     999
#> 2  52       1 2019-02-02   999    <NA>     999
#> 3  55       2 2020-05-17   999     999     999
#> 4  83       3 2015-07-22   999     999     999
#> 5  84       3 2018-01-22   999    <NA>     999
#> 6  45       4 2015-12-06  F130    F114    F144
```

Here’s how you could use `healthdb` to implement the SUD definition
above:

1.  Identify rows contains the target codes in the claim database

    ``` r
    result1 <- claim_db %>%
      identify_row(
    vars = starts_with("diagx_"),
    match = "start",
    vals = c(291:292, 303:305)
      )
    #> ℹ Identify records with condition(s):
    #> • where at least one of the diagx_1, diagx_2 column(s) in each record
    #> • contains a value satisfied SQL LIKE pattern: 291% OR 292% OR 303% OR 304% OR 305%
    #> ℹ To see the final query generated by 'dbplyr', use dplyr::show_query() on the output.
    #> To extract the SQL string, use dbplyr::remote_query().
    ```

2.  Restrict the number of records per client

    ``` r
    result2 <- result1 %>% restrict_n(
      clnt_id = clnt_id,
      n_per_clnt = 2,
      count_by = dates,
      # here we use filter mode to remove records that failed the restriction
      mode = "filter"
    )
    #> ℹ Apply restriction that each client must have at least 2 records with distinct
    #> dates. Clients/groups which did not met the condition were excluded.
    result2 %>% head()
    #> # Source:     SQL [6 x 7]
    #> # Database:   sqlite 3.45.2 [:memory:]
    #> # Ordered by: dates
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>
    #> 1    24       3 17352 291   3047    <NA>                  1
    #> 2     4       3 17554 2913  3032    <NA>                  1
    #> 3    28       4 17750 2929  2925    <NA>                  1
    #> 4     5       4 18518 2918  3035    999                   1
    #> 5     3       7 16765 2921  2910    <NA>                  1
    #> 6    13       7 17738 3058  3048    <NA>                  1
    ```

3.  Restrict the temporal pattern of diagnoses

    ``` r
    result3 <- result2 %>% restrict_date(
      clnt_id = clnt_id,
      date_var = dates,
      n = 2,
      within = 365,
      uid = uid,
      # here we use flag mode to flag records that met the restriction instead of removing those
      mode = "flag"
    )
    #> ℹ Apply restriction that each client must have 2 records that were within 365
    #> days. Records that met the condition were flagged.
    result3 %>% head()
    #> # Source:     SQL [6 x 8]
    #> # Database:   sqlite 3.45.2 [:memory:]
    #> # Ordered by: dates, uid
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n flag_restrict_date
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>              <int>
    #> 1    24       3 17352 291   3047    <NA>                  1                  1
    #> 2     4       3 17554 2913  3032    <NA>                  1                  0
    #> 3    28       4 17750 2929  2925    <NA>                  1                  0
    #> 4     5       4 18518 2918  3035    999                   1                  0
    #> 5     3       7 16765 2921  2910    <NA>                  1                  0
    #> 6    13       7 17738 3058  3048    <NA>                  1                  0
    ```

4.  Repeat these steps for hospitalization and row bind the results.

The output of these functions, including `identify_row()`, `exclude()`,
`restrict_n()`, `restrict_date()` and more, can be piped into ‘dplyr’
functions for further manipulations. Therefore, wrangling with them
along with ‘dplyr’ provide the maximum flexibility for implementing
complex algorithms. However, your code could look repetitive if multiple
data sources were involved. See the “Intro” vignette
(`vignette("healthdb")`) **for a much more concise way to work with
multiple sources and definitions** (the ‘Call-building functions’
section).
