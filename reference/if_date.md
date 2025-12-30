# Interpret if any n elements drawn from a date vector could be some days apart within some time span

Given a vector of dates x, interpret if there could be at least one set
of n elements taken from x satisfy that adjacent elements in the set are
at least certain days apart AND the dates in the set are within the
specified time span. When identifying events/diseases from
administrative data, definitions often require, e.g., n diagnoses that
are at least some days apart within some years. This function is
intended for such use and optimized to avoid looping through all n-size
combinations in x. This function does not work with remote table input.

## Usage

``` r
if_date(
  x,
  n,
  apart = NULL,
  within = NULL,
  detail = FALSE,
  align = c("left", "right"),
  dup.rm = TRUE,
  ...
)
```

## Arguments

- x:

  A character or Date vector

- n:

  An integer for the size of a draw

- apart:

  An integer specifying the minimum gap (in days) between adjacent dates
  in a draw.

- within:

  An integer specifying the maximum time span (in days) of a draw.

- detail:

  Logical for whether return result per element of x.The default is
  FALSE, which returns one logical summarized by any(). Detail is not
  available if `apart` was supplied without `within` because sets that
  satisfied the condition could overlap, and records within a set may be
  far apart; thus, no unambiguous way to label by element.

- align:

  Character, define if the time span for each record should start
  ("left") or end ("right") at its current date. Defaults to "left". See
  'flag_at' argument in
  [`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md)
  for detail.

- dup.rm:

  Logical for whether multiple records on the same date should be count
  as one in calculation. Only applicable when `within` is supplied
  without `apart`; duplicated dates have no impact when `apart` is
  present as the n dates must be distinct if they were apart. Default is
  TRUE.

- ...:

  Additional argument passing to
  [`data.table::as.IDate()`](https://rdatatable.gitlab.io/data.table/reference/IDateTime.html)
  for date conversion.

## Value

Single or a vector of logical for whether there is any draw from x
satisfied the conditions

## See also

[`restrict_date()`](https://kevinhzq.github.io/healthdb/reference/restrict_date.md)

## Examples

``` r
dates_of_records <- sample(seq(as.Date("2015-01-01"), as.Date("2021-12-31"), 7), 10)

# whether there is any 3 records at least 30 days apart within 2 years
if_date(dates_of_records, n = 3, apart = 30, within = 365 * 2)
#> [1] TRUE

# specified either apart or within or both
if_date(dates_of_records, n = 2, within = 365)
#> Warning: 'x' is deprecated in frollapply, use 'X' instead
#> Warning: 'n' is deprecated in frollapply, use 'N' instead
#> [1] TRUE
```
