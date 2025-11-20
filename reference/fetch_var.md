# Get variables from multiple tables with common ID columns

This function fetches variables from different tables that linked by
common IDs. It calls
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
multiple times with various source tables (y argument of the join) to
gather variables. It is not meant to replace left_join() but simplify
syntax for the situation that you started off a table of study sample
and wanted to gather covariates from different sources linked by common
client IDs, which is often the case when working with healthcare
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
#> 1   3 2021-04-06 2021        1        1   F
#> 2   7 2020-05-30 2020        1        0   M
#> 3   7 2020-12-12 2020        1        0   M
#> 4   8 2020-05-01 2020        1        1   M
#> 5   8 2020-08-17 2020        1        1   M
#> 6   4 2020-08-23 2020        1        0   M
#> 7   9 2021-06-10 2021        0        0   M
#> 8   6 2021-06-28 2021        0        1   F
#> 9   2 2021-02-25 2021        1        1   M
#> 10  6 2020-10-30 2020        0        1   F
#> 11 10 2021-10-07 2021        0        0   M
#> 12  8 2020-12-09 2020        1        1   M
#> 13  5 2022-01-29 2022        0        0   F
#> 14  1 2021-09-21 2021        0        1   M
#> 15  2 2021-09-28 2021        1        1   M
#> 16  1 2022-01-22 2022        1        0   M
#> 17  3 2020-08-14 2020        0        0   F
#> 18  6 2021-03-20 2021        0        1   F
#> 19  9 2021-02-10 2021        0        0   M
#> 20  2 2021-05-02 2021        1        1   M
#> 21  6 2021-02-08 2021        0        1   F
#> 22  8 2021-04-30 2021        1        1   M
#> 23  5 2021-02-28 2021        0        1   F
#> 24  3 2020-03-02 2020        0        0   F
#> 25 10 2021-06-06 2021        0        0   M
#> 26  9 2021-01-11 2021        0        0   M
#> 27  7 2020-03-03 2020        1        0   M
#> 28  5 2021-04-17 2021        0        1   F
#> 29  8 2020-04-02 2020        1        1   M
#> 30 10 2021-09-11 2021        0        0   M

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
#> 1   3 2021-04-06 2021        1        1   F
#> 2   7 2020-05-30 2020        1        0   M
#> 3   7 2020-12-12 2020        1        0   M
#> 4   8 2020-05-01 2020        1        1   M
#> 5   8 2020-08-17 2020        1        1   M
#> 6   4 2020-08-23 2020        1        0   M
#> 7   9 2021-06-10 2021        0        0   M
#> 8   6 2021-06-28 2021        0        1   F
#> 9   2 2021-02-25 2021        1        1   M
#> 10  6 2020-10-30 2020        0        1   F
#> 11 10 2021-10-07 2021        0        0   M
#> 12  8 2020-12-09 2020        1        1   M
#> 13  5 2022-01-29 2022        0        0   F
#> 14  1 2021-09-21 2021        0        1   M
#> 15  2 2021-09-28 2021        1        1   M
#> 16  1 2022-01-22 2022        1        0   M
#> 17  3 2020-08-14 2020        0        0   F
#> 18  6 2021-03-20 2021        0        1   F
#> 19  9 2021-02-10 2021        0        0   M
#> 20  2 2021-05-02 2021        1        1   M
#> 21  6 2021-02-08 2021        0        1   F
#> 22  8 2021-04-30 2021        1        1   M
#> 23  5 2021-02-28 2021        0        1   F
#> 24  3 2020-03-02 2020        0        0   F
#> 25 10 2021-06-06 2021        0        0   M
#> 26  9 2021-01-11 2021        0        0   M
#> 27  7 2020-03-03 2020        1        0   M
#> 28  5 2021-04-17 2021        0        1   F
#> 29  8 2020-04-02 2020        1        1   M
#> 30 10 2021-09-11 2021        0        0   M
```
