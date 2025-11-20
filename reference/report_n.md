# Report number of distinct value in a column across data frames

This function is intended to mimic
[`dplyr::n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
for multiple inputs. It is useful to report the number of clients
through out a series of inclusion or exclusion steps. An use case could
be getting the Ns for the sample definition flowchart in an
epidemiological study. It is also useful for inline reporting of Ns in a
Rmarkdown document.

## Usage

``` r
report_n(..., on, force_proceed = getOption("healthdb.force_proceed"))
```

## Arguments

- ...:

  Data frames or remote tables (e.g., from 'dbplyr')

- on:

  The column to report on. It must be present in all data sources.

- force_proceed:

  A logical for whether to ask for user input in order to proceed when
  the data is not local data.frames, and a query needs to be executed
  before reporting. The default is fetching from options (FALSE). Use
  `options(healthdb.force_proceed = TRUE)` to suppress the prompt once
  and for all.

## Value

A sequence of the number of distinct `on` for each data frames

## Examples

``` r
# some exclusions
iris_1 <- subset(iris, Petal.Length > 1)
iris_2 <- subset(iris, Petal.Length > 2)

# get n at each operation
n <- report_n(iris, iris_1, iris_2, on = Species)
n
#> [1] 3 3 2

# get the difference at each step
diff(n)
#> [1]  0 -1
# data in a list
iris_list <- list(iris_1, iris_2)
report_n(rlang::splice(iris_list), on = Species)
#> [1] 3 2
# if you loaded tidyverse, this will also work
# report_n(!!!iris_list, on = Species)
```
