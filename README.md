
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthdb \<a href=“<https://kevinhzq.github.io/healthdb/>”<img src="man/figures/logo.png" alt="healthdb website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/KevinHzq/healthdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KevinHzq/healthdb/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KevinHzq/healthdb/branch/master/graph/badge.svg)](https://app.codecov.io/gh/KevinHzq/healthdb?branch=master)

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

Administrative health data data are often stored on database with strict
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
devtools::install_github("KevinHzq/healthdb")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo KevinHzq/healthdb@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\kevin.hu\AppData\Local\Temp\Rtmpy2N36w\remotes53b0514b5843\KevinHzq-healthdb-6855b83/DESCRIPTION' ...  ✔  checking for file 'C:\Users\kevin.hu\AppData\Local\Temp\Rtmpy2N36w\remotes53b0514b5843\KevinHzq-healthdb-6855b83/DESCRIPTION'
#>       ─  preparing 'healthdb': (376ms)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>       ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'healthdb_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/kevin.hu/AppData/Local/Temp/RtmpeEZNbu/temp_libpath5a6c8601a70'
#> (as 'lib' is unspecified)
```

## Example

Case definition: One or more hospitalization with a substance use
disorder (SUD) diagnostic code, OR Two or more physician claims with a
substance use disorder diagnostic code within one year.

We are going to implement this definition. First, let’s make a demo data
sets for the two sources:

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
#> 1    51       1 16660 999   999     999    
#> 2    24       1 17464 3048  3040    <NA>   
#> 3    14       2 17640 3041  3047    <NA>   
#> 4    37       3 16948 2913  3035    999    
#> 5    91       4 16712 999   999     999    
#> 6    82       4 16760 999   999     <NA>
```

Hospitalization

``` r
hosp_df <- make_test_dat(vals_kept = c(str_glue("F{10:19}"), str_glue("F{100:199}"), noise_val = "999"), type = "data.frame")

# this is a local data.frame/tibble
hosp_df %>% head()
#>   uid clnt_id      dates diagx diagx_1 diagx_2
#> 1  43       1 2015-08-13  F156    F144    F168
#> 2  16       1 2017-10-25   F12    F132    <NA>
#> 3   6       2 2018-04-19  F133    F128    <NA>
#> 4  29       3 2016-05-27  F130    F164     999
#> 5  83       4 2015-10-04   999     999    <NA>
#> 6  74       4 2015-11-21   999    <NA>     999
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
    #> 
    #> Identify records with condition(s): 
    #>  - where at least one of the diagx_1, diagx_2 column(s) in each record 
    #>    - contains a value satisfied SQL LIKE pattern: 291% OR 292% OR 303% OR 304% OR 305% 
    #> 
    #> To see the final query generated by 'dbplyr', use dplyr::show_query() on the output. 
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
    #> 
    #> Apply restriction that each client must have at least 2 records with distinct dates. Clients/groups whichdid not meetthe condition were excluded.
    result2 %>% head()
    #> # Source:     SQL [6 x 7]
    #> # Database:   sqlite 3.45.2 [:memory:]
    #> # Ordered by: dates
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>
    #> 1    35       7 16810 2913  2923    999                   1
    #> 2    22       7 16897 3056  2917    999                   1
    #> 3    47       7 17096 3033  3051    3036                  1
    #> 4    10       7 17250 2923  3057    999                   1
    #> 5    41      10 16954 3033  305     2929                  1
    #> 6    44      10 17788 999   304     292                   1
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
    #> 
    #> Apply restriction that each client must have 2 records that were  within 365 days. Records that met the condition were flagged.
    result3 %>% head()
    #> # Source:     SQL [6 x 8]
    #> # Database:   sqlite 3.45.2 [:memory:]
    #> # Ordered by: dates, uid
    #>     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n flag_restrict_date
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>             <int>              <int>
    #> 1    35       7 16810 2913  2923    999                   1                  1
    #> 2    22       7 16897 3056  2917    999                   1                  1
    #> 3    47       7 17096 3033  3051    3036                  1                  1
    #> 4    10       7 17250 2923  3057    999                   1                  0
    #> 5    41      10 16954 3033  305     2929                  1                  0
    #> 6    44      10 17788 999   304     292                   1                  0
    ```

4.  Repeat these steps for hospitalization and row bind the results.

The output of these functions, including `identify_row()`, `exclude()`,
`restrict_n()`, `restrict_date()` and more can be piped into ‘dplyr’
functions for further manipulations. Therefore, wrangling with them
along with ‘dplyr’ provide the maximum flexibility for implementing
complex algorithms. However, your code could look repetitive if multiple
data sources were involved. See the “Intro” vignette
(`vignette("Intro")`) for a much more concise way to work with multiple
sources and definitions (the ‘Call-building functions’ section).
