# Row-bind a list of data.frames or remote tables with variable selection and renaming

Row bind a list of data.frames or remote tables with variable selection
and renaming.

## Usage

``` r
bind_source(data, ..., force_proceed = getOption("healthdb.force_proceed"))
```

## Arguments

- data:

  A list of data.frame or remote tables, e.g., output from
  [`execute_def()`](https://kevinhzq.github.io/healthdb/reference/execute_def.md).

- ...:

  Named arguments for each variable included in the output. The argument
  name should be the new name in the output, and the right hand side of
  the argument is a character vector of the original names. The name
  vector and the list elements in `data` will be matched by position. if
  an output variable only came from some of the sources, fill the name
  vector to a length equal to the number of sources with NA, e.g., `var`
  only come from the second out of three sources, then
  `var = c(NA, 'nm_in_src2', NA)`.

- force_proceed:

  A logical for whether to ask for user input in order to proceed when
  remote tables are needed to be collected for binding. The default is
  FALSE to let user be aware of that the downloading process may be
  slow. Use `options(healthdb.force_proceed = TRUE)` to suppress the
  prompt once and for all.

## Value

A data.frame or remote table containing combined rows of the input list
with variables specified by ...

## Examples

``` r
df1 <- subset(iris, Species == "setosa")
df2 <- subset(iris, Species == "versicolor")
df3 <- subset(iris, Species == "virginica")

bind_source(list(df1, df2, df3),
  s_l = "Sepal.Length",
  s_w = "Sepal.Width",
  p_l_setosa = c("Petal.Length", NA, NA),
  p_l_virginica = c(NA, NA, "Petal.Length")
) %>%
  head()
#>   src_No s_l s_w p_l_setosa p_l_virginica
#> 1      1 5.1 3.5        1.4            NA
#> 2      1 4.9 3.0        1.4            NA
#> 3      1 4.7 3.2        1.3            NA
#> 4      1 4.6 3.1        1.5            NA
#> 5      1 5.0 3.6        1.4            NA
#> 6      1 5.4 3.9        1.7            NA
```
