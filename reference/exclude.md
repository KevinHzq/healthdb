# Remove rows based on conditions or another data set

This function combines
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
and negation of
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
When a second data set is supplied through the `excl` argument, anti
join would be performed; otherwise, `data` would be filtered with the
expression given via the `condition` argument, and the filter result
would in turn be removed using
[`dplyr::setdiff()`](https://dplyr.tidyverse.org/reference/setops.html).

## Usage

``` r
exclude(
  data,
  excl = NULL,
  by = NULL,
  condition = NULL,
  verbose = getOption("healthdb.verbose"),
  report_on = NULL,
  ...
)
```

## Arguments

- data:

  Data.frames or remote tables (e.g., from
  [`dbplyr::tbl_sql()`](https://dbplyr.tidyverse.org/reference/tbl_sql.html)).
  A subset will be removed from this data.

- excl:

  Data frames or remote tables (e.g., from 'dbplyr'). Rows/values
  present in it will be removed from `data` if there is a match. This
  will be passed to
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  as the second argument.

- by:

  Column names that should be matched by
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
  or a expressions with
  [`dplyr::join_by()`](https://dplyr.tidyverse.org/reference/join_by.html).
  See
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)'s
  `by` argument for detail. Default NULL is the same as
  `setdiff(data, excl)`.

- condition:

  An expression that will be passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
  The rows that satisfy `condition` are those to be removed from `data`.

- verbose:

  A logical for whether printing explanation for the operation. Default
  is fetching from options. Use `options(healthdb.verbose = FALSE)` to
  suppress once and for all.

- report_on:

  A quoted/unquoted column name for counting how many of its distinct
  values were removed from `data`, e.g., counting how many client IDs
  were removed. Default is NULL.

- ...:

  Additional arguments passing to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)/[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  for finer control of matching, e.g., na action, by-group filtering,
  etc.

## Value

A data frame or remote table that is a subset of `data`.

## Examples

``` r
# exclude with condition
cyl_not_4 <- exclude(mtcars, condition = cyl == 4, report_on = cyl)
#> ℹ Exclude a subset of `data` that satisfies condition: cyl == 4 
#> ℹ Consider being explicit about NA, e.g., condition = var == 'val' | is.na(var)
#> ℹ Of the 3 cyl in data, 1 were excluded.

# exclude with another data
exclude(mtcars, cyl_not_4, dplyr::join_by(cyl), report_on = cyl)
#> ℹ Exclude records in `data` through anti_join with `excl` matching on (by argument): dplyr::join_by(cyl) 
#> ℹ Of the 3 cyl in data, 2 were excluded.
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```
