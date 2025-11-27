# Identify diseases/events from administrative records with age restriction

This function extends the standard case definition function
[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md)
by allowing age-based filtering. See
[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md)
for more general description of what this function does.

Note that when using this function with an existing age variable, the
age should be determined at the time of the record. Records that are not
in the eligible age range will be remove before interpreting the
temporal relationship between records. In other words, the age
restriction is applied before
[`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).

For other age restrictions based on a fixed time point (e.g., age at the
baseline of follow-up), it can be done by filtering the input data or
output of
[`define_case()`](https://kevinhzq.github.io/healthdb/reference/define_case.md)
instead of using this function.

## Usage

``` r
define_case_with_age(
  data,
  vars,
  match = "in",
  vals,
  clnt_id,
  n_per_clnt = 1,
  date_var = NULL,
  apart = NULL,
  within = NULL,
  uid = NULL,
  excl_vals = NULL,
  excl_args = NULL,
  keep = c("all", "first", "last"),
  if_all = FALSE,
  mode = c("flag", "filter"),
  birth_date = NULL,
  age = NULL,
  age_range = NULL,
  force_collect = FALSE,
  verbose = getOption("healthdb.verbose"),
  ...
)
```

## Arguments

- data:

  Data.frames or remote tables (e.g., from
  [`dbplyr::tbl_sql()`](https://dbplyr.tidyverse.org/reference/tbl_sql.html))

- vars:

  An expression passing to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  It can be quoted/unquoted column names, or helper functions, such as
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html).

- match:

  One of "in", "start", "regex", "like", "between", and "glue_sql". It
  determines how values would be matched. See
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_row.md)
  for detail.

- vals:

  Depending on `match`, it takes different input. See
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_row.md).

- clnt_id:

  Grouping variable (quoted/unquoted).

- n_per_clnt:

  A single number specifying the minimum number of group size.

- date_var:

  Variable name (quoted/unquoted) for the dates to be interpreted.

- apart:

  An integer specifying the minimum gap (in days) between adjacent dates
  in a draw.

- within:

  An integer specifying the maximum time span (in days) of a draw.

- uid:

  Variable name for a unique row identifier. It is necessary for SQL to
  produce consistent result based on sorting.

- excl_vals:

  Same as `vals` but clients/groups with these values are going to be
  removed from the result. This is intended for exclusion criteria of a
  case definition.

- excl_args:

  A named list of arguments passing to the second
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_row.md)
  call for `excl_vals`. If not supplied, `var`, `match` and `if_all` of
  the first call will be re-used.

- keep:

  One of:

  - "first" (keeping each client's earliest record),

  - "last" (keeping the latest),

  - and "all" (keeping all relevant records, default).

  - Note that "first"/"last" should not be used with "flag" mode.

- if_all:

  A logical for whether combining the predicates (if multiple columns
  were selected by vars) with AND instead of OR. Default is FALSE, e.g.,
  var1 in vals OR var2 in vals.

- mode:

  Either:

  - "flag" - add new columns starting with "flag\_" indicating if the
    client met the condition,

  - or "filter" - remove clients that did not meet the condition from
    the data.

  - This will be passed to both
    [`restrict_n()`](https://kevinhzq.github.io/healthdb/reference/restrict_n.md)
    AND
    [`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).
    Default is "flag".

- birth_date:

  Optional. The name of the column containing birth dates. Used to
  calculate age when `age_range` is specified. Requires `date_var` to be
  supplied. Age will be calculated as (date_var - birth_date)/365.25.

- age:

  Optional. The name of the column containing age values. Used directly
  for age filtering when `age_range` is specified.

- age_range:

  Optional. A length 2 numeric vector `c(min, max)` specifying the age
  range in years. Use `NA` for one-sided bounds (e.g., `c(10, NA)` for
  age \>= 10, or `c(NA, 65)` for age \<= 65). At least one non-NA value
  must be provided.

- force_collect:

  A logical for whether force downloading the result table if it is not
  a local data.frame. Downloading data could be slow, so the user has to
  opt in; default is FALSE.

- verbose:

  A logical for whether printing explanation for the operation. Default
  is fetching from options. Use `options(healthdb.verbose = FALSE)` to
  suppress once and for all.

- ...:

  Additional arguments, e.g., `mode`, passing to
  [`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).

## Value

A subset of input data satisfied the specified case definition.

## Examples

``` r
sample_size <- 30
df <- data.frame(
  clnt_id = rep(1:3, each = 10),
  service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1),
    size = sample_size, replace = TRUE
  ),
  diagx = sample(letters, size = sample_size, replace = TRUE),
  diagx_1 = sample(c(NA, letters), size = sample_size, replace = TRUE),
  diagx_2 = sample(c(NA, letters), size = sample_size, replace = TRUE)
)

# define from one source
define_case_with_age(df,
  vars = starts_with("diagx"), "in", vals = letters[1:4],
  clnt_id = clnt_id, date_var = service_dt,
  excl_args = list(if_all = TRUE),
  # remove non-case
  mode = "filter",
  # keeping the first record
  keep = "first"
)
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: letters[1:4]
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    a    b    c    d    e    h    i    j    o    p    r    t    u    w    x    z 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> NA's 
#>    1 
#> → -------------- Output first records--------------
#> # A tibble: 3 × 5
#>   clnt_id service_dt diagx diagx_1 diagx_2
#>     <int> <date>     <chr> <chr>   <chr>  
#> 1       1 2020-01-03 j     u       a      
#> 2       2 2020-01-12 o     d       e      
#> 3       3 2020-01-07 j     c       x      

# with age restriction using birth_date
df_with_birth <- df
df_with_birth$birth_dt <- as.Date("1990-01-01")
define_case_with_age(df_with_birth,
  vars = starts_with("diagx"), "in", vals = letters[1:4],
  clnt_id = clnt_id, date_var = service_dt,
  birth_date = birth_dt, age_range = c(18, 65),
  mode = "filter"
)
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: letters[1:4]
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    a    b    c    d    e    h    i    j    o    p    r    t    u    w    x    z 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> NA's 
#>    1 
#> → --------------Age restriction--------------
#> → -------------- Output all records--------------
#>    clnt_id service_dt diagx diagx_1 diagx_2   birth_dt
#> 1        1 2020-01-16     c       w       c 1990-01-01
#> 2        1 2020-01-25     t       p       b 1990-01-01
#> 3        1 2020-01-03     j       u       a 1990-01-01
#> 4        2 2020-01-16     o       h       d 1990-01-01
#> 5        2 2020-01-12     o       d       e 1990-01-01
#> 6        2 2020-01-27     b       u       j 1990-01-01
#> 7        2 2020-01-18     z       z       a 1990-01-01
#> 8        3 2020-01-26     b       j       b 1990-01-01
#> 9        3 2020-01-16     d       i       h 1990-01-01
#> 10       3 2020-01-09     b       r    <NA> 1990-01-01
#> 11       3 2020-01-07     j       c       x 1990-01-01

# age restriction with one-sided bound (age >= 18 only)
define_case_with_age(df_with_birth,
  vars = starts_with("diagx"), "in", vals = letters[1:4],
  clnt_id = clnt_id, date_var = service_dt,
  birth_date = birth_dt, age_range = c(18, NA),
  mode = "filter"
)
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: letters[1:4]
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    a    b    c    d    e    h    i    j    o    p    r    t    u    w    x    z 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> NA's 
#>    1 
#> → --------------Age restriction--------------
#> → -------------- Output all records--------------
#>    clnt_id service_dt diagx diagx_1 diagx_2   birth_dt
#> 1        1 2020-01-16     c       w       c 1990-01-01
#> 2        1 2020-01-25     t       p       b 1990-01-01
#> 3        1 2020-01-03     j       u       a 1990-01-01
#> 4        2 2020-01-16     o       h       d 1990-01-01
#> 5        2 2020-01-12     o       d       e 1990-01-01
#> 6        2 2020-01-27     b       u       j 1990-01-01
#> 7        2 2020-01-18     z       z       a 1990-01-01
#> 8        3 2020-01-26     b       j       b 1990-01-01
#> 9        3 2020-01-16     d       i       h 1990-01-01
#> 10       3 2020-01-09     b       r    <NA> 1990-01-01
#> 11       3 2020-01-07     j       c       x 1990-01-01

# multiple sources with purrr::pmap
# arguments with length = 1 will be recycle to match the number of sources
# wrap expressions/unquoted variables with bquote(),
# or rlang:exprs() to prevent immediate evaluation,
# or just use quoted variable names
purrr::pmap(
  list(
    data = list(df, df),
    vars = rlang::exprs(starts_with("diagx")),
    match = c("in", "start"),
    vals = list(letters[1:4], letters[5:10]),
    clnt_id = list(bquote(clnt_id)), n_per_clnt = c(2, 3),
    date_var = "service_dt",
    excl_vals = list(letters[11:13], letters[14:16]),
    excl_args = list(list(if_all = TRUE), list(if_all = FALSE))
  ),
  define_case_with_age
)
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: c("a", "b", "c", "d")
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    a    b    c    d    e    h    i    j    o    p    r    t    u    w    x    z 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#> NA's 
#>    1 
#> → --------------Exclusion step--------------
#> ℹ Identify records with condition(s):
#> • where all of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: c("k", "l", "m")
#> 
#> All unique value(s) and frequency in the result : 
#> integer(0)
#> ℹ Exclude records in `data` through anti_join with `excl` matching on (by argument): "clnt_id" 
#> ℹ Of the 3 "clnt_id" in data, 0 were excluded.
#> → --------------No. rows restriction--------------
#> ℹ Of the 3 clients in the input, 0 were flagged as 0 by restricting that each client must have at least 2 records with distinct service_dt
#> → -------------- Output all records--------------
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value satisfied regular expression: ^e|^f|^g|^h|^i|^j
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    a    b    c    d    e    f    g    h    i    j    k    l    o    p    r    t 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#>    u    w    x    y    z NA's 
#>    1    1    1    1    1    1 
#> → --------------Exclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value satisfied regular expression: ^n|^o|^p
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>    b    d    e    h    i    j    l    m    n    o    p    q    r    s    t    v 
#>    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
#>    x    y    z NA's 
#>    1    1    1    1 
#> ℹ Exclude records in `data` through anti_join with `excl` matching on (by argument): "clnt_id" 
#> ℹ Of the 3 "clnt_id" in data, 3 were excluded.
#> → --------------No. rows restriction--------------
#> ℹ Of the 0 clients in the input, 0 were flagged as 0 by restricting that each client must have at least 3 records with distinct service_dt
#> → -------------- Output all records--------------
#> [[1]]
#>    clnt_id service_dt diagx diagx_1 diagx_2 flag_restrict_n
#> 1        1 2020-01-16     c       w       c               1
#> 2        1 2020-01-25     t       p       b               1
#> 3        1 2020-01-03     j       u       a               1
#> 4        2 2020-01-16     o       h       d               1
#> 5        2 2020-01-12     o       d       e               1
#> 6        2 2020-01-27     b       u       j               1
#> 7        2 2020-01-18     z       z       a               1
#> 8        3 2020-01-26     b       j       b               1
#> 9        3 2020-01-16     d       i       h               1
#> 10       3 2020-01-09     b       r    <NA>               1
#> 11       3 2020-01-07     j       c       x               1
#> 
#> [[2]]
#> [1] clnt_id         service_dt      diagx           diagx_1        
#> [5] diagx_2         flag_restrict_n
#> <0 rows> (or 0-length row.names)
#> 
```
