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

  A logical for whether to transform both `from` and `to` with the
  `.transfn` function

- .transfn:

  A function for transforming the inputs. Default is
  [`lubridate::ymd()`](https://lubridate.tidyverse.org/reference/ymd.html).

- verbose:

  A logical for whether to print a summary of the output and warn about
  missing values. Default is fetching from options. Use
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
#>   25.89   28.48   33.43   35.22   40.45   47.85 
#>   <19 19-24 25-34 35-44 45-54   55+ 
#>     0     0     3     1     1     0 
#>   id   start_dt     end_dt age_grp
#> 1  1 1994-07-08 2005-12-17   25-34
#> 2  2 1997-02-11 2002-08-27   25-34
#> 3  3 1975-02-25 2018-07-09   45-54
#> 4  4 1989-07-26 2019-08-06   25-34
#> 5  5 1982-07-20 2010-08-02   35-44

# compute gaps between two dates in weeks
df %>% dplyr::mutate(
  gap_wks = compute_duration(start_dt, end_dt, unit = "week")
)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   289.0   597.1  1462.9  1235.7  1566.9  2262.9 
#>   id   start_dt     end_dt   gap_wks
#> 1  1 1994-07-08 2005-12-17  597.1429
#> 2  2 1997-02-11 2002-08-27  289.0000
#> 3  3 1975-02-25 2018-07-09 2262.8571
#> 4  4 1989-07-26 2019-08-06 1566.8571
#> 5  5 1982-07-20 2010-08-02 1462.8571
```
