# Introduction to healthdb

------------------------------------------------------------------------

## What it does

- This package is designed for identifying disease cases from admin data
  for epidemiological studies. The implementation focused on code
  readability and re-usability. Three types of functions are included:

- *Interactive functions* (e.g.,
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_row.md),
  [`exclude()`](https://kevinhzq.github.io/healthdb/reference/exclude.md),
  [`fetch_var()`](https://kevinhzq.github.io/healthdb/reference/fetch_var.md))
  based on filter and joins from dplyr with tweaks that fix SQL
  translation or add features that are not natively support by SQL. They
  also work for local data.frame, and some use ‘data.table’ package
  ([`vignette("datatable-intro", package = "data.table")`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html))
  to speed up processing time for large data. These functions are not as
  flexible as
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  but they are general enough to be useful even outside health research.

- *Call-building functions* (e.g.,
  [`build_def()`](https://kevinhzq.github.io/healthdb/reference/build_def.md),
  [`execute_def()`](https://kevinhzq.github.io/healthdb/reference/execute_def.md))
  that facilitate batch execution and re-use of case definitions. In
  essence, `build_def` creates codes of definitions (which is chain of
  the interactive functions, e.g.,
  [`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md))
  that are not immediately ran. `execute_def` runs built definitions
  with different input data.

- *Miscellaneous functions* such as computing age
  [`compute_duration()`](https://kevinhzq.github.io/healthdb/reference/compute_duration.md),
  collapsing records within a time range into one episode
  [`collapse_episode()`](https://kevinhzq.github.io/healthdb/reference/collapse_episode.md),
  and more (on-going effort). Most of these functions have built-in
  checks signalling when things might go wrong, e.g., missing values in
  calculated ages.

### Motivation

In health research and surveillance, identifying diseases or events from
administrative databases is often the initial step. However, crafting
case-finding algorithms is a complex task. Existing algorithms, often
written in SAS by experienced analysts, can be complex and difficult to
decipher for the growing number of analysts trained primarily in R.

These algorithms may also affect performance if they depend on Data Step
in SAS, due to a lack of translation between Data Step and SQL. This can
result in SAS downloading data from a remote database to a local
machine, leading to poor performance when handling large,
population-based databases.

The ‘healthdb’ R package was created to address these challenges. It
minimizes the need to download data and offers an easy-to-use interface
for working with healthcare databases. It also includes capabilities not
supported by ‘SQL’, such as matching strings by ‘stringr’ style regular
expressions, and can compute comorbidity scores
([`compute_comorbidity()`](https://kevinhzq.github.io/healthdb/reference/compute_comorbidity.md))
directly on a database server. This vignette will present an example of
common use cases.

------------------------------------------------------------------------

## Installation

Simply run:

``` r
install.packages("healthdb")
```

We will need the following packages for this demo.

``` r
library(dplyr)
library(dbplyr)
library(lubridate)
library(glue)
library(purrr)
library(healthdb)
```

## Intended use case

Consider the case definition of substance use disorder (SUD) from
[British Columbia Centre for Disease Control’s Chronic Disease
Dashboard](http://www.bccdc.ca/resource-gallery/Documents/Chronic-Disease-Dashboard/substance-use-disorder.pdf),

> One or more hospitalization with a substance use disorder diagnostic
> code, OR Two or more physician visits with a substance use disorder
> diagnostic code within one year.

We are going to implement this definition. First, let’s make a demo data
sets for the two sources:

1.  Physician claims with multiple columns of
    [ICD-9](https://www2.gov.bc.ca/gov/content/health/practitioner-professional-resources/msp/physicians/diagnostic-code-descriptions-icd-9)
    diagnostic codes

    ``` r
    # make_test_dat() makes either a toy data.frame or database table in memory with known number of rows that satisfy the query we will show later
    claim_db <- make_test_dat(vals_kept = c("303", "304", "305", "291", "292", glue("30{30:59}"), glue("29{10:29}"), noise_val = c("999", "111")), type = "database")

    # this is a database table
    # note that in-memory SQLite database stores dates as numbers
    claim_db %>% head()
    #> # Source:   SQL [?? x 6]
    #> # Database: sqlite 3.51.1 [:memory:]
    #>     uid clnt_id dates diagx diagx_1 diagx_2
    #>   <int>   <int> <dbl> <chr> <chr>   <chr>  
    #> 1    25       3 17909 3030  2921    NA     
    #> 2    46       4 18497 2925  3047    2925   
    #> 3    79       5 16559 999   999     NA     
    #> 4    40       5 17134 3056  3042    NA     
    #> 5    63       5 17213 999   999     999    
    #> 6    77       5 17976 999   NA      NA
    ```

2.  Hospitalization with [ICD-10](https://en.wikipedia.org/wiki/ICD-10)
    codes

    ``` r
    hosp_df <- make_test_dat(vals_kept = c(glue("F{10:19}"), glue("F{100:199}"), noise_val = "999"), type = "data.frame")

    # this is a local data.frame/tibble
    hosp_df %>% head()
    #>   uid clnt_id      dates diagx diagx_1 diagx_2
    #> 1 100       1 2015-02-01   999     999     999
    #> 2  85       1 2017-07-17   999    <NA>     999
    #> 3   3       1 2019-06-17  F133    F159     999
    #> 4  65       2 2019-01-08   999    <NA>     999
    #> 5  52       2 2019-06-01   999    <NA>    <NA>
    #> 6  75       3 2016-07-11   999    <NA>    <NA>

    # convert Date to numeric to be consistent with claim_db
    hosp_df <- hosp_df %>%
      mutate(dates = julian(dates))
    ```

## Interactive functions

Let’s focus on the physician claims. Extracting clients with at least
two records within a year is not difficult, and involves only a few
steps. The codes could look like the following using dplyr, however, it
does not work because: 1. SQL does not support multiple patterns in one
LIKE operation, 2. dbply currently have issue with translating
n_distinct.

``` r
## not run
claim_db %>%
  # identify the target codes
  filter(if_any(starts_with("diagx"), ~ str_like(., c("291%", "292%", "303%", "304%", "305%")))) %>%
  # each clnt has at least 2 records on different dates
  group_by(clnt_id) %>%
  # the n_distinct step is mainly for reducing computation in the next step
  filter(n_distinct(dates) >= 2) %>%
  # any two dates within one year?
  filter((max(dates) - min(dates)) <= 365)
## end
```

Here’s how you could use `healthdb` to achieve these steps:

1.  Identify rows contains the target codes. Use
    [`?identify_row`](https://kevinhzq.github.io/healthdb/reference/identify_row.md)
    to see a list of supported matching types.

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

2.  Bonus: remove clients with exclusion codes

    This step is not in the substance use disorder definition, but other
    disease definitions often require exclusion of some ICDs that
    contradicts the ones of interest. Let’s say we want to remove
    clients with code “111” here.

    We first identify “111” from the source, then exclude clients in the
    output from the previous step’s result.
    [`exclude()`](https://kevinhzq.github.io/healthdb/reference/exclude.md)
    take either a data set (via the excl argument) or expression
    (condition argument) as input. For the former, it performs an anti
    join matching on the by argument (see
    [`dplyr::join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)).
    For the latter, it is the opposite of filter, i.e.,
    `filter(!(some_expression))`.

    ``` r
    result2 <- result1 %>%
      exclude(
    excl = identify_row(claim_db, starts_with("diagx"), "in", "111"),
    by = "clnt_id"
      )
    #> ℹ Identify records with condition(s):
    #> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
    #> • contains a value exactly matched values in set: "111"
    #> ℹ Exclude records in `data` through anti_join with `excl` matching on (by argument): "clnt_id"
    ```

3.  Restrict the number of records per client

    ``` r
    result3 <- result2 %>% restrict_n(
      clnt_id = clnt_id,
      n_per_clnt = 2,
      count_by = dates,
      # here we use filter mode to remove records that failed the restriction
      mode = "filter"
    )
    #> ℹ Apply restriction that each client must have at least 2 records with
    #> distinct dates. Clients/groups which did not met the condition were excluded.
    ```

4.  Restrict the temporal pattern of diagnoses

    [`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md)
    supports more complicated patterns like having n diagnoses at least
    i days apart within j years. Note that when SQL interpret order of
    dates, the result could be not deterministic if there were duplicate
    dates within client. Therefore, a unique row id (uid) has to be
    supplied to get consistent result.

    ``` r
    result4 <- result3 %>% restrict_date(
      clnt_id = clnt_id,
      date_var = dates,
      n = 2,
      within = 365,
      uid = uid,
      # here we use flag mode to flag records that met the restriction instead of removing those
      mode = "flag"
    )
    #> ℹ Apply restriction that each client must have 2 records that were
    #> within 365 days. Records that met the condition were flagged.
    ```

5.  Fetch variables from other tables by matching common keys

    Up to this point, the result is only a query and have not been
    downloaded. Hopefully, the data has been shrunken to a manageable
    size for collection.

    ``` r
    # Class of result4
    class(result4)
    #> [1] "tbl_SQLiteConnection" "tbl_dbi"              "tbl_sql"             
    #> [4] "tbl_lazy"             "tbl"

    # execute query and download the result
    result_df <- result4 %>% collect()

    # Number of rows in source
    nrow(claim_db %>% collect())
    #> [1] 100

    # Number of rows in the current result
    nrow(result_df)
    #> [1] 27
    ```

    Our data now only contains diagnoses which are probably not enough
    for further analyses. Let’s say we want to gather client
    demographics such as age and sex from other sources. This certainly
    can be done with multiple
    [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
    calls. Here we provide the
    [`fetch_var()`](https://kevinhzq.github.io/healthdb/reference/fetch_var.md)
    function to make the codes more concise. Note that the input must be
    a named object and not from a pipe (i.e., don’t do this
    `data %>% some_action %>% fetch_var()`).

    ``` r
    # make two look up tables
    age_tab <- data.frame(
      clnt_id = 1:50,
      age = sample(1:90, 50),
      sex = sample(c("F", "M"), 50, replace = TRUE)
    )
    address_tab <- data.frame(
      clnt_id = rep(1:50, 5), year = rep(2016:2020, each = 50),
      area_code = sample(0:200, 50, replace = TRUE)
    )

    # get year from dates for matching

    result_df <- result_df %>% mutate(year = lubridate::year(as.Date(dates, origin = "1970-01-01")))

    # note that keys must be present in all tables
    fetch_var(result_df,
      keys = c(clnt_id, year),
      linkage = list(
    # the formula means from_table ~ get_variable
    # |clnt_id means matching on clnt_id only
    age_tab ~ c(age, sex) | clnt_id,
    address_tab ~ area_code
      )
    ) %>%
      select(uid, clnt_id, dates, age, sex, area_code) %>%
      head()
    #> # A tibble: 6 × 6
    #>     uid clnt_id dates   age sex   area_code
    #>   <int>   <int> <dbl> <int> <chr>     <int>
    #> 1     1      18 16988     4 M           193
    #> 2    34      18 17810     4 M           193
    #> 3    45      20 16517    62 M            NA
    #> 4    39      20 17305    62 M           154
    #> 5    33      21 16451     8 F            NA
    #> 6    50      21 16818     8 F           196
    ```

## Call-building functions

To complete the definition, we need to repeat the process shown above
with hospitalization data. Some studies may use more than a handful of
data sources to define their sample. We packed steps 1-4 in one function
[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md),
and provide tools to perform batch execution with different data and
parameters to meet those needs.

``` r
# build the full definition of SUD
sud_def <- build_def(
  # name of definition
  def_lab = "SUD",
  # place holder names for sources
  src_labs = c("claim", "hosp"),
  def_fn = define_case, # you could alter it and supply your own function
  # below are argumets of define_case
  fn_args = list(
    # if length = 1, the single element will be use for every source
    vars = list(starts_with("diagx")),
    match = "start", # match ICD starts with vals
    vals = list(c(291:292, 303:305), glue("F{10:19}")),
    clnt_id = clnt_id,
    n_per_clnt = c(2, 1),
    date_var = dates,
    within = c(365, NULL),
    uid = uid,
    mode = "flag"
  )
)

sud_def
#> # A tibble: 2 × 5
#>   def_lab src_labs def_fn      fn_args          fn_call   
#>   <chr>   <chr>    <chr>       <list>           <list>    
#> 1 SUD     claim    define_case <named list [9]> <language>
#> 2 SUD     hosp     define_case <named list [9]> <language>
```

Let’s look inside the fn_call list column. Two calls of
[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md)
have been made with different parameters. The data arguments are left
empty on purpose for re-usability. For example, you may want to repeat
the analysis with data from different regions or study periods.

``` r
sud_def$fn_call
#> [[1]]
#> define_case(data = , vars = starts_with("diagx"), match = "start", 
#>     vals = c(291:292, 303:305), clnt_id = clnt_id, n_per_clnt = 2, 
#>     date_var = dates, within = 365, uid = uid, mode = "flag")
#> 
#> [[2]]
#> define_case(data = , vars = starts_with("diagx"), match = "start", 
#>     vals = glue("F{10:19}"), clnt_id = clnt_id, n_per_clnt = 1, 
#>     date_var = dates, within = NULL, uid = uid, mode = "flag")
```

Executing the definition is simply a call of
[`execute_def()`](https://kevinhzq.github.io/healthdb/reference/execute_def.md).
If verbose option is not turned off by
`options(healthdb.verbose = FALSE)`, the output message will explain
what has been done. You could append multiple
[`build_def()`](https://kevinhzq.github.io/healthdb/reference/build_def.md)
outputs together and execute them all at once. Definition and source
labels will be added to the result to identify outputs from different
calls.

``` r
# execute the definition
result_list <- sud_def %>%
  execute_def(with_data = list(
    claim = claim_db,
    hosp = hosp_df
  ))
#> 
#> Actions for definition SUD using source claim_db:
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value satisfied SQL LIKE pattern: 291% OR 292% OR 303% OR 304% OR 305%
#> → --------------No. rows restriction--------------
#> 
#> ℹ Apply restriction that each client must have at least 2 records with distinct dates. Records that met the condition were flagged.
#> → --------------Time span restriction--------------
#> 
#> ℹ Apply restriction that each client must have 2 records that were within 365 days. Records that met the condition were flagged.
#> → -------------- Output all records--------------
#> 
#> 
#> Actions for definition SUD using source hosp_df:
#> 
#> → --------------Inclusion step--------------
#> 
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value satisfied regular expression: ^F10|^F11|^F12|^F13|^F14|^F15|^F16|^F17|^F18|^F19
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>  999  F10 F101 F103 F104 F105 F107 F108 F109 F111 F112 F113 F114 F115 F116 F117 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> F118 F119  F12 F121 F122 F123 F124 F125 F128 F129  F13 F132 F133 F134 F135 F137 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> F138  F14 F140 F141 F143 F144 F145 F146 F147  F15 F154 F155 F159  F16 F160 F161 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> F162 F164 F165 F166 F168  F17 F170 F172 F173 F174 F175 F176 F179 F181 F183 F184 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> F185 F187 F188 F189  F19 F191 F192 F193 F196 F197 F198 NA's 
#>    1    1    1    1    1    1    1    1    1    1    1    1
#> → -------------- Output all records--------------
```

Let’s check the results!

``` r
# view the results
purrr::walk(result_list, ~ head(.) %>% print())
#> # Source:   SQL [?? x 10]
#> # Database: sqlite 3.51.1 [:memory:]
#>   def   src     uid clnt_id dates diagx diagx_1 diagx_2 flag_restrict_n
#>   <chr> <chr> <int>   <int> <dbl> <chr> <chr>   <chr>             <int>
#> 1 SUD   claim    25       3 17909 3030  2921    NA                    0
#> 2 SUD   claim    46       4 18497 2925  3047    2925                  0
#> 3 SUD   claim    40       5 17134 3056  3042    NA                    0
#> 4 SUD   claim    10       8 17237 2912  3057    999                   0
#> 5 SUD   claim    41      12 17520 3047  3049    2915                  0
#> 6 SUD   claim    22      13 18489 3045  3042    NA                    0
#> # ℹ 1 more variable: flag_restrict_date <int>
#>   def  src uid clnt_id dates diagx diagx_1 diagx_2
#> 1 SUD hosp   3       1 18064  F133    F159     999
#> 2 SUD hosp  38       4 16625  F191    F147     999
#> 3 SUD hosp  17       4 17279  F114    F154     999
#> 4 SUD hosp  30       5 16614  F101    F107    <NA>
#> 5 SUD hosp  27       5 17079  F165    F174    <NA>
#> 6 SUD hosp  24       5 17414  F118    F111    <NA>
```

At this point, the result from the claim database (`result[[1]]`) has
not been collected locally. You could collect it manually, do further
filtering, and then combine with the result from hospitalization data in
any way you want. If you just need a simple row bind, we have
[`bind_source()`](https://kevinhzq.github.io/healthdb/reference/bind_source.md)
with convenient naming feature.

``` r
bind_source(result_list,
  # output_name = c(names in the list elements)
  src = "src",
  uid = "uid",
  clnt_id = "clnt_id",
  flag = c("flag_restrict_date", NA),
  # force_proceed is needed to collect remote tables to local memory
  force_proceed = TRUE
)
#> # A tibble: 100 × 5
#>    src_No src     uid clnt_id  flag
#>     <int> <chr> <int>   <int> <int>
#>  1      1 claim    25       3     0
#>  2      1 claim    46       4     0
#>  3      1 claim    40       5     0
#>  4      1 claim    10       8     0
#>  5      1 claim    41      12     0
#>  6      1 claim    22      13     0
#>  7      1 claim     3      15     0
#>  8      1 claim    14      16     0
#>  9      1 claim    26      17     0
#> 10      1 claim     1      18     0
#> # ℹ 90 more rows
```

[`pool_case()`](https://kevinhzq.github.io/healthdb/reference/pool_case.md)
goes a few steps further than row bind. It also filters records with
valid flags and can summarize by client/group. Since we had to decide
which variables to be summarized in advance, the output may not be
flexible enough to meet your needs.

``` r
pool_case(result_list,
  def = sud_def,
  # your could skip summary with output_lvl = "raw"
  output_lvl = "clnt",
  # include records only from sources having valid records, see function documentation for more detail and other options
  include_src = "has_valid",
  force_proceed = TRUE
)
#> # A tibble: 29 × 10
#>    def   clnt_id first_valid_date first_valid_src last_entry_date last_entry_src
#>    <chr>   <int>            <dbl> <chr>                     <dbl> <chr>         
#>  1 SUD         1            18064 hosp                      18064 hosp          
#>  2 SUD         4            16625 hosp                      17279 hosp          
#>  3 SUD         5            16614 hosp                      17414 hosp          
#>  4 SUD         7            17700 hosp                      17865 hosp          
#>  5 SUD         8            17821 hosp                      18135 hosp          
#>  6 SUD         9            17637 hosp                      18344 hosp          
#>  7 SUD        11            17675 hosp                      18395 hosp          
#>  8 SUD        12            17888 hosp                      17888 hosp          
#>  9 SUD        13            18342 hosp                      18342 hosp          
#> 10 SUD        16            18088 hosp                      18088 hosp          
#> # ℹ 19 more rows
#> # ℹ 4 more variables: raw_in_claim <dbl>, raw_in_hosp <dbl>,
#> #   valid_in_claim <int>, valid_in_hosp <int>
```
