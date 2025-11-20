# Cut the time period in one row into multiple rows by interval

This function is for cutting time periods into segments, which could be
useful for subsequent overlap joins. Each original period (per row) will
be expanded to multiple rows by weeks, months, etc. Only data.frame
input is accepted as the output size is greater than the input. Thus,
remote tables should be collected before running this function for
optimal performance.

## Usage

``` r
cut_period(
  data,
  start,
  end,
  len,
  unit = c("day", "week", "month", "quarter", "year"),
  .dt_trans = NULL
)
```

## Arguments

- data:

  Input data.frame that each row has start and end dates

- start:

  Record start date column (unquoted)

- end:

  Record end date column (unquoted)

- len:

  An integer, the interval that would be used to divide the record
  duration

- unit:

  One of "day" (default), "week", "month", "quarter, or "year" used in
  combination of `len` to specify the time length of the interval.

- .dt_trans:

  Function to transform start/end, such as
  [`lubridate::ymd()`](https://lubridate.tidyverse.org/reference/ymd.html).
  Default is NULL.

## Value

Data frame that each row is now a segment of the period defined by
`c(start, end)` in the original row. Original variables are retained and
repeated for each segment plus new variables defining the segment
interval.

## Examples

``` r
# toy data
df <- data.frame(sample_id = 1, period_id = 1, start_date = "2015-01-01", end_date = "2019-12-31")

# divide period into segments (multiple rows per period)
df_seg <- cut_period(
  data = df, start = start_date, end = end_date,
  len = 1,
  unit = "year",
  .dt_trans = lubridate::ymd
)

# categorize segment_id as factor
df_seg$segment <- cut(df_seg$segment_id,
  breaks = c(0, 1, 2, Inf),
  labels = c("< 1 year", "1 - 2 years", "Remainder")
)

head(df_seg)
#> # A tibble: 5 × 8
#>   sample_id period_id start_date end_date   segment_start segment_end segment_id
#>       <dbl>     <dbl> <date>     <date>     <date>        <date>           <int>
#> 1         1         1 2015-01-01 2019-12-31 2015-01-01    2015-12-31           1
#> 2         1         1 2015-01-01 2019-12-31 2016-01-01    2016-12-31           2
#> 3         1         1 2015-01-01 2019-12-31 2017-01-01    2017-12-31           3
#> 4         1         1 2015-01-01 2019-12-31 2018-01-01    2018-12-31           4
#> 5         1         1 2015-01-01 2019-12-31 2019-01-01    2019-12-31           5
#> # ℹ 1 more variable: segment <fct>
```
