# Remove or flag groups with n less than some number

Remove or flags groups or clients that have less than some number of
rows or some number of distinct values in a variable. For example, it
can be used to remove clients that had less than n visits to some
service on different dates from some administrative records. It offers
filtering with
[`dplyr::n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
functionality for database input.

## Usage

``` r
restrict_n(
  data,
  clnt_id,
  n_per_clnt,
  count_by = NULL,
  mode = c("flag", "filter"),
  verbose = getOption("healthdb.verbose")
)
```

## Arguments

- data:

  Data.frames or remote tables (e.g., from
  [`dbplyr::tbl_sql()`](https://dbplyr.tidyverse.org/reference/tbl_sql.html))

- clnt_id:

  Grouping variable (quoted/unquoted).

- n_per_clnt:

  A single number specifying the minimum number of group size.

- count_by:

  Another variable dictating the counting unit of `n_per_clnt.` The
  default is NULL meaning the inclusion criteria is the number of row,
  i.e., `dplyr::n() >= n_per_clnt`. If it is not NULL, the criteria
  becomes equivalent to `dplyr::n_distinct(count_by) >= n_per_clnt`.

- mode:

  Either "flag" - add a new column 'flag_restrict_n' indicating if the
  client met the condition (all rows from a qualified client would have
  flag = 1), or "filter" - remove clients that did not meet the
  condition from the data. Default is "flag".

- verbose:

  A logical for whether to explain the query and report how many groups
  were removed. Default is fetching from options. Use
  `options(healthdb.verbose = FALSE)` to suppress once and for all.
  Reporting is not for remote tables as the query is not executed
  immediately, thus no result is available for summary without adding an
  extra run (may be slow) of the query.

## Value

A subset of input data satisfied the group size requirement, or raw
input data with an new flag column.

## See also

[`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html),
[`dplyr::n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)

## Examples

``` r
# flag cyl groups with less than 8 cars
restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 8, mode = "flag") %>%
head()
#> ℹ Of the 3 clients in the input, 1 were flagged as 0 by restricting that each client must have at least 8 records 
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb flag_restrict_n
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4               0
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4               0
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1               1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1               0
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2               1
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1               0

#remove cyl groups with less than 2 types of gear boxes
restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 3, count_by = gear, mode = "filter")
#> ℹ Of the 3 clients in the input, 1 were excluded by restricting that each client must have at least 3 records with distinct gear
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb flag_restrict_n
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4               1
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4               1
#> 3  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1               1
#> 4  18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1               1
#> 5  19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4               1
#> 6  17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4               1
#> 7  19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6               1
#> 8  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1               1
#> 9  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2               1
#> 10 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2               1
#> 11 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1               1
#> 12 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2               1
#> 13 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1               1
#> 14 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1               1
#> 15 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1               1
#> 16 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2               1
#> 17 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2               1
#> 18 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2               1
```
