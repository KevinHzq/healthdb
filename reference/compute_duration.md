# Compute duration between two dates

This function is meant to be for data frame input only and used with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
to compute age or duration between two character or Date columns. If a
vector of breaks is given, the output will be converted to factor with
labels generated automatically.

## Usage

``` r
compute_duration(
  from,
  to,
  lower_brks = NULL,
  unit = c("year", "day", "week", "month"),
  trans = FALSE,
  .transfn = lubridate::ymd,
  verbose = getOption("healthdb.verbose"),
  ...
)
```

## Arguments

- from:

  A character or Date vector for start dates.

- to:

  A character or Date vector for end dates.

- lower_brks:

  A numeric vector for lower breaks passing to the base
  [`base::cut()`](https://rdrr.io/r/base/cut.html) function to convert
  the numeric result to a factor. The level labels will be auto
  generated. For example, the level labels are
  `c("<19", "19-24", "25-34", "35-44", "45-54", "55+")` for
  `lower_brks = c(0, 19, 25, 35, 45, 55)`. Default is NULL (no
  conversion).

- unit:

  A character string specifying the unit of the output. One of "year"
  (default), "day", "week", or "month".

- trans:

  A logical for whether transform both `from` and `to` with the
  `.transfn` function

- .transfn:

  A function for transforming the inputs. Default is
  [`lubridate::ymd()`](https://lubridate.tidyverse.org/reference/ymd.html).

- verbose:

  A logical for whether print summary of the out and warning for missing
  values. Default is fetching from options. Use
  `options(healthdb.verbose = FALSE)` to suppress once and for all.

- ...:

  Additional arguments passing to
  [`base::cut()`](https://rdrr.io/r/base/cut.html).

## Value

A numeric or factor vector of the duration.

## Examples

``` r
# toy data
n <- 5
df <- data.frame(
  id = 1:n,
  start_dt = sample(seq(as.Date("1970-01-01"), as.Date("2000-12-31"), by = 1), size = n),
  end_dt = sample(seq(as.Date("2001-01-01"), as.Date("2023-12-31"), by = 1), size = n)
)

# get age group at a cut-off
df %>% dplyr::mutate(
  age_grp = compute_duration(start_dt, "2023-01-01", lower_brks = c(0, 19, 25, 35, 45, 55))
)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   28.50   34.41   43.42   39.98   44.86   48.72 
#>   <19 19-24 25-34 35-44 45-54   55+ 
#>     0     0     2     2     1     0 
#>   id   start_dt     end_dt age_grp
#> 1  1 1988-08-05 2018-12-14   25-34
#> 2  2 1979-08-02 2021-10-26   35-44
#> 3  3 1974-04-14 2004-05-31   45-54
#> 4  4 1978-02-21 2023-12-28   35-44
#> 5  5 1994-07-02 2007-12-07   25-34

# compute gaps between two dates in weeks
df %>% dplyr::mutate(
  gap_wks = compute_duration(start_dt, end_dt, unit = "week")
)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   700.9  1572.1  1584.0  1690.6  2203.7  2392.3 
#>   id   start_dt     end_dt   gap_wks
#> 1  1 1988-08-05 2018-12-14 1584.0000
#> 2  2 1979-08-02 2021-10-26 2203.7143
#> 3  3 1974-04-14 2004-05-31 1572.1429
#> 4  4 1978-02-21 2023-12-28 2392.2857
#> 5  5 1994-07-02 2007-12-07  700.8571
```
