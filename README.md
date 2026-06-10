
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
diseases or events from healthcare databases and preparing data for
epidemiological studies. It features abilities that are not natively
supported by databases, such as matching strings by ‘stringr’ style
regular expressions and using the ‘LIKE’ operator with multiple patterns
in a vector. Three types of functions are included: interactive
functions – for customizing complex definitions; call building functions
– for batch execution of simple definitions; miscellaneous functions –
for data wrangling, computing age and comorbidity index, etc.

**The package is automatically tested against SQLite, PostgreSQL, and
SQL Server with every update.** Please report bugs if you encounter
issues with other SQL dialects.

Administrative health data are often stored in SQL databases with strict
security measures which may disable permission to write temporary
tables. Writing queries without being able to cache intermediate results
is challenging, especially when the data is too large to be downloaded
from the database into R (i.e., local memory) without some filtering
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
#> # Source:   SQL [?? x 6]
#> # Database: sqlite 3.53.1 [:memory:]
#>     uid clnt_id dates diagx diagx_1 diagx_2
#>   <int>   <int> <dbl> <chr> <chr>   <chr>  
#> 1     3       1 16780 2914  2926    <NA>   
#> 2    10       1 17594 305   3050    999    
#> 3    93       1 17688 999   <NA>    999    
#> 4    43       1 18287 3043  3031    3033   
#> 5    37       1 18419 2921  2919    <NA>   
#> 6    50       2 16806 3041  3039    2910
```

Hospitalization

``` r
hosp_df <- make_test_dat(vals_kept = c(str_glue("F{10:19}"), str_glue("F{100:199}"), noise_val = "999"), type = "data.frame")

# this is a local data.frame/tibble
hosp_df %>% head()
#>   uid clnt_id      dates diagx diagx_1 diagx_2
#> 1  52       1 2018-08-31   999     999     999
#> 2  11       1 2019-02-05  F154    F163     999
#> 3   6       1 2020-02-29  F179    F174    <NA>
#> 4  96       2 2017-03-17   999     999     999
#> 5  37       2 2019-11-15  F153    F142     999
#> 6  25       2 2020-10-20  F197    F153    <NA>
```

Here’s how you could use `healthdb` to implement the SUD definition
above:

1.  Identify rows contains the target codes in the claim database

    ``` r
    result1 <- claim_db %>%
      identify_row(
    vars = starts_with("diagx"),
    match = "start",
    vals = c(291:292, 303:305)
      )
    #> ℹ Identify records with condition(s):
    #> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
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
    #> # Source:   SQL [?? x 7]
    #> # Database: sqlite 3.53.1 [:memory:]
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>
    #> 1     3       1 16780 2914  2926    <NA>                  1
    #> 2    10       1 17594 305   3050    999                   1
    #> 3    43       1 18287 3043  3031    3033                  1
    #> 4    37       1 18419 2921  2919    <NA>                  1
    #> 5    50       2 16806 3041  3039    2910                  1
    #> 6    46       2 17003 2911  2914    3046                  1
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
    #> # Source:   SQL [?? x 8]
    #> # Database: sqlite 3.53.1 [:memory:]
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n flag_restrict_date
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>              <int>
    #> 1     3       1 16780 2914  2926    <NA>                  1                  0
    #> 2    10       1 17594 305   3050    999                   1                  0
    #> 3    43       1 18287 3043  3031    3033                  1                  1
    #> 4    37       1 18419 2921  2919    <NA>                  1                  0
    #> 5    50       2 16806 3041  3039    2910                  1                  1
    #> 6    46       2 17003 2911  2914    3046                  1                  1
    ```

4.  Repeat these steps for hospitalization and row bind the results.

The output of these functions, including `identify_row()`, `exclude()`,
`restrict_n()`, `restrict_date()` and more, can be piped into ‘dplyr’
functions for further manipulations. Therefore, wrangling with them
along with ‘dplyr’ provide the maximum flexibility for implementing
complex algorithms. However, your code could look repetitive if multiple
data sources were involved. See the introduction vignette
(`vignette("healthdb")`) **for a much more concise way to work with
multiple sources and definitions** (the ‘Call-building functions’
section).
