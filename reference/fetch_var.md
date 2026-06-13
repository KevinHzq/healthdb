# Get variables from multiple tables with common ID columns

This function fetches variables from different tables that are linked by
common IDs. It calls
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
multiple times with various source tables (y argument of the join) to
gather variables. It is not meant to replace left_join() but simplify
syntax for the situation that you started off with a table of your study
sample and wanted to gather covariates from different sources linked by
common client IDs, which is often the case when working with healthcare
databases. **Caution**: this function is intended for one-to-one joins
only because it could be problematic when we do not know which source
caused a one-to-many join and changed the number of rows. For data.frame
input, an error will be given when one-to-many joins were detected.
However, such checking could be an expensive operation on remote source.
Therefore, for database input, the result will not be checked.

## Usage

``` r
fetch_var(data, keys, linkage, ...)
```

## Arguments

- data:

  A data.frame or remote table (tbl_sql) which must be an object and not
  from a pipe. It would be used as the x argument in left_join().

- keys:

  A vector of quoted/unquoted variable names, or 'tidyselect' expression
  (see
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)).
  These variables must be present in `data` and would be used as the
  `by` argument in left_join(). The y tables must have a subset of these
  if not all.

- linkage:

  A list of formulas in the form of "from_tab ~ get_vars\|by_keys":

  - source table on the left-hand-side

  - variables on the right-hand-side

  - If a source table does not have all the variables in `keys`, use
    "\|" on RHS to specify the subset of `keys` to be used.

  For example, given `keys` has 3 variables, list( y1 ~
  tidyselect_expr1, y2 ~ tidyselect_expr2\|key1 + key2)

  meaning:

  1.  from table y1 get variables picked by the tidyselect expression
      matching on all 3 keys;

  2.  from table y2 get variables matching on only key1 and key2.

- ...:

  Additional arguments, e.g., `copy = TRUE`, passing to left_join().

## Value

A data.frame or remote table containing all original columns of x and
new variables matched from other tables based on the specified linkage.

## Examples

``` r
# make toy data
size <- 30
n <- 10
df1 <- data.frame(
  id = sample(1:n, size = size, replace = TRUE),
  service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2022-01-31"), by = 1),
    size = size
  )
) %>%
  dplyr::mutate(year = lubridate::year(service_dt))
df2 <- data.frame(
  id = rep(1:n, size / n), year = rep(2020:2022, each = n),
  status_1 = sample(0:1, size = size, replace = TRUE),
  status_2 = sample(0:1, size = size, replace = TRUE)
)
df3 <- data.frame(id = 1:n, sex = sample(c("F", "M"), size = n, replace = TRUE))

# simple joins
# note that for left_join(df1, df2), boths keys have to be used,
# otherwise, error as the relation would not be one-to-one
fetch_var(df1,
  keys = c(id, year),
  linkage = list(
    df2 ~ starts_with("s"), # match both keys without '|'
    df3 ~ sex | id
  ) # match by id only; otherwise failed because df3 has no year
)
#>    id service_dt year status_1 status_2 sex
#> 1   2 2021-11-14 2021        1        0   M
#> 2   3 2020-06-26 2020        0        0   F
#> 3   9 2022-01-24 2022        0        0   M
#> 4   5 2021-09-28 2021        1        1   F
#> 5   8 2020-04-18 2020        1        0   F
#> 6   6 2021-11-19 2021        0        1   M
#> 7   1 2020-01-14 2020        1        0   F
#> 8   4 2020-05-20 2020        0        1   M
#> 9  10 2021-09-10 2021        1        1   F
#> 10  3 2020-05-17 2020        0        0   F
#> 11  2 2020-09-17 2020        1        1   M
#> 12  7 2021-11-02 2021        0        1   F
#> 13 10 2022-01-10 2022        1        0   F
#> 14 10 2021-01-29 2021        1        1   F
#> 15  2 2020-03-11 2020        1        1   M
#> 16  5 2021-07-20 2021        1        1   F
#> 17  6 2021-07-21 2021        0        1   M
#> 18 10 2020-11-10 2020        0        0   F
#> 19  7 2020-02-13 2020        0        0   F
#> 20  6 2020-07-25 2020        0        1   M
#> 21  3 2020-03-09 2020        0        0   F
#> 22  4 2021-05-31 2021        0        0   M
#> 23  3 2020-01-13 2020        0        0   F
#> 24  6 2020-11-16 2020        0        1   M
#> 25  4 2021-12-25 2021        0        0   M
#> 26 10 2020-05-03 2020        0        0   F
#> 27  7 2020-09-16 2020        0        0   F
#> 28  9 2020-01-11 2020        1        0   M
#> 29  9 2020-02-01 2020        1        0   M
#> 30  1 2020-08-25 2020        1        0   F

# example if some y is remote
# make df2 as database table
db2 <- dbplyr::tbl_memdb(df2)

fetch_var(df1,
  keys = c(id, year),
  linkage = list(
    db2 ~ starts_with("s"),
    df3 ~ sex | id
  ),
  copy = TRUE # pass to left_join for forced collection of remote table
)
#>    id service_dt year status_1 status_2 sex
#> 1   2 2021-11-14 2021        1        0   M
#> 2   3 2020-06-26 2020        0        0   F
#> 3   9 2022-01-24 2022        0        0   M
#> 4   5 2021-09-28 2021        1        1   F
#> 5   8 2020-04-18 2020        1        0   F
#> 6   6 2021-11-19 2021        0        1   M
#> 7   1 2020-01-14 2020        1        0   F
#> 8   4 2020-05-20 2020        0        1   M
#> 9  10 2021-09-10 2021        1        1   F
#> 10  3 2020-05-17 2020        0        0   F
#> 11  2 2020-09-17 2020        1        1   M
#> 12  7 2021-11-02 2021        0        1   F
#> 13 10 2022-01-10 2022        1        0   F
#> 14 10 2021-01-29 2021        1        1   F
#> 15  2 2020-03-11 2020        1        1   M
#> 16  5 2021-07-20 2021        1        1   F
#> 17  6 2021-07-21 2021        0        1   M
#> 18 10 2020-11-10 2020        0        0   F
#> 19  7 2020-02-13 2020        0        0   F
#> 20  6 2020-07-25 2020        0        1   M
#> 21  3 2020-03-09 2020        0        0   F
#> 22  4 2021-05-31 2021        0        0   M
#> 23  3 2020-01-13 2020        0        0   F
#> 24  6 2020-11-16 2020        0        1   M
#> 25  4 2021-12-25 2021        0        0   M
#> 26 10 2020-05-03 2020        0        0   F
#> 27  7 2020-09-16 2020        0        0   F
#> 28  9 2020-01-11 2020        1        0   M
#> 29  9 2020-02-01 2020        1        0   M
#> 30  1 2020-08-25 2020        1        0   F
```
