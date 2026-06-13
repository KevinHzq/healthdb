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
#>   25.45   25.67   40.37   35.99   41.58   46.86 
#>   <19 19-24 25-34 35-44 45-54   55+ 
#>     0     0     2     2     1     0 
#>   id   start_dt     end_dt age_grp
#> 1  1 1976-02-23 2008-02-28   45-54
#> 2  2 1982-08-18 2010-03-26   35-44
#> 3  3 1997-05-01 2006-06-04   25-34
#> 4  4 1997-07-20 2006-08-09   25-34
#> 5  5 1981-06-04 2010-02-14   35-44

# compute gaps between two dates in weeks
df %>% dplyr::mutate(
  gap_wks = compute_duration(start_dt, end_dt, unit = "week")
)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   472.4   474.4  1440.3  1111.0  1497.4  1670.4 
#>   id   start_dt     end_dt   gap_wks
#> 1  1 1976-02-23 2008-02-28 1670.4286
#> 2  2 1982-08-18 2010-03-26 1440.2857
#> 3  3 1997-05-01 2006-06-04  474.4286
#> 4  4 1997-07-20 2006-08-09  472.4286
#> 5  5 1981-06-04 2010-02-14 1497.4286
```
