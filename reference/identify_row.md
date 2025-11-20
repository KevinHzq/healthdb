# Identify rows with a match

Filter rows which values satisfy the specified conditions. The
functionality is identical to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
combined with
[`dplyr::if_any()`](https://dplyr.tidyverse.org/reference/across.html)
or
[`dplyr::if_all()`](https://dplyr.tidyverse.org/reference/across.html),
but it used the 'data.table' package
[`vignette("datatable-intro", package = "data.table")`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
for data.frame method, and has regular regular expression support for
remote database tables. The motivation is to take away some pain when
working with databases which often do not support regular expression and
'LIKE' operator with multiple string patterns.

## Usage

``` r
identify_row(
  data,
  vars,
  match = c("in", "start", "regex", "like", "between", "glue_sql"),
  vals,
  if_all = FALSE,
  verbose = getOption("healthdb.verbose"),
  query_only = TRUE,
  ...
)
```

## Arguments

- data:

  Data.frames or remote tables (e.g., from
  [`dbplyr::tbl_sql()`](https://dbplyr.tidyverse.org/reference/tbl_sql.html))

- vars:

  An expression passing to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  It can be quoted/unquoted column names, or helper functions, such as
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html).

- match:

  One of "in", "start", "regex", "like", "between", and "glue_sql". It
  determines how values would be matched. The operations under each
  type:

  - "in": var %in% vals (This is default)

  - "regex": stringr::str_detect(var, vals). For remote tables, unique
    values in vars are collected locally before matching (may be slow).

  - "like": stringr::str_like(var, vals). For remote tables, WHERE var
    LIKE val.

  - "start": same as regex or LIKE with modified vals, e.g.,
    "^val1\|^val2" or "va1%\|val2%"

  - "between": dplyr::between(var, val1, val2)

  - "glue_sql": For remote table only, this gives full control of the
    WHERE clause using dplyr::filter(dbplyr::sql(glue::glue_sql(...)))

- vals:

  Depending on `match`, it takes different input:

  - "in": a vector of values (numeric/character/Date)

  - "start": a vector of numeric/character that would be modified into a
    regex or LIKE pattern string by adding "^" in front or "%" at the
    end

  - "regex"/"like": a string of the expression

  - "between": a vector of numeric or date with exactly two elements,
    e.g., c(lower, upper)

  - "glue_sql": a string of a SQL WHERE clause, which will be passed to
    [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html).
    See examples for detail.

- if_all:

  A logical for whether combining the predicates (if multiple columns
  were selected by vars) with AND instead of OR. Default is FALSE, e.g.,
  var1 in vals OR var2 in vals.

- verbose:

  A logical for whether printing explanation and result overview for the
  query. Default is fetching from options. Use
  `options(healthdb.verbose = FALSE)` to suppress once and for all.
  Result overview is not for remote tables as the query is not executed
  immediately, thus no result is available for summary without adding an
  extra run (may be slow) of the query.

- query_only:

  A logical for whether keeping the output as remote table (Default
  TRUE) or downloading the query result as a tibble (FALSE). The
  argument is ignored when the input data is a data.frame/tibble.

- ...:

  For remote table method only. Additional arguments passing to
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  for parameterized queries.

## Value

A data.frame or tbl_sql object depending on the input.

## Examples

``` r
# applying to data.frame; both sepal length and width in range 3-5
identify_row(iris, starts_with("Sepal"), "between", c(3, 5), if_all = TRUE)
#> ℹ Identify records with condition(s):
#> • where all of the Sepal.Length, Sepal.Width column(s) in each record
#> • contains a value between range (bounds included): c(3, 5)
#> 
#> Summary of values in the result : 
#> Range: [1] 3 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1           4.9         3.0          1.4         0.2  setosa
#> 2           4.7         3.2          1.3         0.2  setosa
#> 3           4.6         3.1          1.5         0.2  setosa
#> 4           5.0         3.6          1.4         0.2  setosa
#> 5           4.6         3.4          1.4         0.3  setosa
#> 6           5.0         3.4          1.5         0.2  setosa
#> 7           4.9         3.1          1.5         0.1  setosa
#> 8           4.8         3.4          1.6         0.2  setosa
#> 9           4.8         3.0          1.4         0.1  setosa
#> 10          4.3         3.0          1.1         0.1  setosa
#> 11          4.6         3.6          1.0         0.2  setosa
#> 12          4.8         3.4          1.9         0.2  setosa
#> 13          5.0         3.0          1.6         0.2  setosa
#> 14          5.0         3.4          1.6         0.4  setosa
#> 15          4.7         3.2          1.6         0.2  setosa
#> 16          4.8         3.1          1.6         0.2  setosa
#> 17          4.9         3.1          1.5         0.2  setosa
#> 18          5.0         3.2          1.2         0.2  setosa
#> 19          4.9         3.6          1.4         0.1  setosa
#> 20          4.4         3.0          1.3         0.2  setosa
#> 21          5.0         3.5          1.3         0.3  setosa
#> 22          4.4         3.2          1.3         0.2  setosa
#> 23          5.0         3.5          1.6         0.6  setosa
#> 24          4.8         3.0          1.4         0.3  setosa
#> 25          4.6         3.2          1.4         0.2  setosa
#> 26          5.0         3.3          1.4         0.2  setosa

# applying to remote table; species starts with se or ends with ca
iris_db <- dbplyr::memdb_frame(iris)
identify_row(iris_db, Species, "like", c("se%", "%ca"))
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL LIKE pattern: se% OR %ca
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
#>  1          4.3         3            1.1         0.1 setosa 
#>  2          4.4         2.9          1.4         0.2 setosa 
#>  3          4.4         3            1.3         0.2 setosa 
#>  4          4.4         3.2          1.3         0.2 setosa 
#>  5          4.5         2.3          1.3         0.3 setosa 
#>  6          4.6         3.1          1.5         0.2 setosa 
#>  7          4.6         3.2          1.4         0.2 setosa 
#>  8          4.6         3.4          1.4         0.3 setosa 
#>  9          4.6         3.6          1           0.2 setosa 
#> 10          4.7         3.2          1.3         0.2 setosa 
#> # ℹ more rows

# using glue_sql to write the WHERE clause
# use {`vars`} to refer to the variables selected by vars
# supply additional values required in the query through '...'
# note that if you use LIKE here, you cannot supply multiple patterns in what
identify_row(iris_db, Species, "glue_sql",
  "{`vars`} LIKE {what}",
  what = "se%"
)
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL WHERE clause: `Species` LIKE 'se%'
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ more rows

# add * after a vector
identify_row(iris_db, Species, "glue_sql",
  "{`vars`} IN ({what*})",
  what = c("setosa", "virginica")
)
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL WHERE clause: `Species` IN ('setosa', 'virginica')
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.51.0 [:memory:]
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # ℹ more rows
```
