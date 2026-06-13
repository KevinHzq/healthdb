# Identify diseases/events from administrative records

This function is a composite of
[`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_rows.md),
[`exclude()`](https://kevinhzq.github.io/healthdb/reference/exclude.md),
[`restrict_n()`](https://kevinhzq.github.io/healthdb/reference/restrict_n.md),
and
[`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).
It aims to implement common case definitions in epidemiological studies
using administrative database as a one-shot big query. The intended use
case is for definitions in the form of, e.g., two or more physician
visits with some diagnostic code at least 30 days apart within two
years. The component functions mentioned above are chained in the
following order if all arguments were supplied:
`identify_row(vals) %>% exclude(identify_row(excl_vals), by = clnt_id) %>% restrict_n() %>% restrict_date()`.
Only the necessary steps in the chain will be run if some arguments are
missing, see the verbose output for what was done. Note that if
`date_var` is supplied, `n_per_clnt` will be counted by distinct dates
instead of number of records.

If your case definition also restricts records to an age range (e.g.,
only count visits made when the client was aged 18-65), use
[`define_case_with_age()`](https://kevinhzq.github.io/healthdb/reference/define_case_with_age.md),
which extends this function with `birth_date`/`age` and `age_range`
arguments.

## Usage

``` r
define_case(
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
  [`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).

- match:

  One of "in", "start", "regex", "like", "between", and "glue_sql". It
  determines how values would be matched. See
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_rows.md)
  for detail.

- vals:

  Depending on `match`, it takes different input. See
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_rows.md).

- clnt_id:

  Grouping variable (quoted/unquoted).

- n_per_clnt:

  A single number specifying the minimum group size.

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
  [`identify_row()`](https://kevinhzq.github.io/healthdb/reference/identify_rows.md)
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

- force_collect:

  A logical for whether force downloading the result table if it is not
  a local data.frame. Downloading data could be slow, so the user has to
  opt in; default is FALSE.

- verbose:

  A logical for whether to print an explanation of the operation.
  Default is fetching from options. Use
  `options(healthdb.verbose = FALSE)` to suppress once and for all.

- ...:

  Additional arguments, e.g., `flag_at`, passing to
  [`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md).

## Value

A subset of the input data that satisfied the specified case definition.

## See also

[`define_case_with_age()`](https://kevinhzq.github.io/healthdb/reference/define_case_with_age.md)
for the same workflow with an added age restriction.

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
define_case(df,
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
#>   a   b   c   d   e   f   g   h   i   j   k   p   q   r   s   t   v   w   x   y 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> NAs 
#>   1 
#> → -------------- Output first records--------------
#> # A tibble: 3 × 5
#>   clnt_id service_dt diagx diagx_1 diagx_2
#>     <int> <date>     <chr> <chr>   <chr>  
#> 1       1 2020-01-02 a     e       r      
#> 2       2 2020-01-10 f     d       v      
#> 3       3 2020-01-01 y     d       k      

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
  define_case
)
#> → --------------Inclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value exactly matched values in set: c("a", "b", "c", "d")
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#>   a   b   c   d   e   f   g   h   i   j   k   p   q   r   s   t   v   w   x   y 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> NAs 
#>   1 
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
#>   a   b   c   d   e   f   g   h   i   j   k   l   p   r   t   v   w   y NAs 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
#> → --------------Exclusion step--------------
#> ℹ Identify records with condition(s):
#> • where at least one of the diagx, diagx_1, diagx_2 column(s) in each record
#> • contains a value satisfied regular expression: ^n|^o|^p
#> 
#> All unique value(s) and frequency in the result (as the conditions require just one of the columns containing target values; irrelevant values may come from other vars columns): 
#> c d e h j l o p q s v y 
#> 1 1 1 1 1 1 1 1 1 1 1 1 
#> ℹ Exclude records in `data` through anti_join with `excl` matching on (by argument): "clnt_id" 
#> ℹ Of the 3 "clnt_id" in data, 3 were excluded.
#> → --------------No. rows restriction--------------
#> ℹ Of the 0 clients in the input, 0 were flagged as 0 by restricting that each client must have at least 3 records with distinct service_dt
#> → -------------- Output all records--------------
#> [[1]]
#>    clnt_id service_dt diagx diagx_1 diagx_2 flag_restrict_n
#> 1        1 2020-01-16     h       a       h               1
#> 2        1 2020-01-02     a       e       r               1
#> 3        1 2020-01-21     c       d       x               1
#> 4        1 2020-01-06     b       f       t               1
#> 5        1 2020-01-09     w       r       b               1
#> 6        2 2020-01-31     c       p       q               1
#> 7        2 2020-01-10     f       d       v               1
#> 8        2 2020-01-21     d       p       s               1
#> 9        2 2020-01-17     a       e       c               1
#> 10       2 2020-01-20     g    <NA>       a               1
#> 11       3 2020-01-01     y       d       k               1
#> 12       3 2020-01-06     b       v       r               1
#> 13       3 2020-01-19     a    <NA>       i               1
#> 14       3 2020-01-14     a       c       k               1
#> 15       3 2020-01-18     i       a       t               1
#> 16       3 2020-01-05     j       g       c               1
#> 
#> [[2]]
#> [1] clnt_id         service_dt      diagx           diagx_1        
#> [5] diagx_2         flag_restrict_n
#> <0 rows> (or 0-length row.names)
#> 
```
