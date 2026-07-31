# Identify rows with a match

Filter rows whose values satisfy the specified conditions. The
functionality is identical to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
combined with
[`dplyr::if_any()`](https://dplyr.tidyverse.org/reference/across.html)
or
[`dplyr::if_all()`](https://dplyr.tidyverse.org/reference/across.html),
but it uses the 'data.table' package
([`vignette("datatable-intro", package = "data.table")`](https://cran.rstudio.com/web/packages/data.table/vignettes/datatable-intro.html))
for the data.frame method, and it supports regular expressions for
remote database tables. The motivation is to take away some pain when
working with databases, which often do not support regular expressions
or the 'LIKE' operator with multiple string patterns.

## Usage

``` r
identify_rows(
  data,
  vars,
  match = c("in", "start", "regex", "like", "between", "glue_sql"),
  vals,
  if_all = FALSE,
  ignore_case = TRUE,
  verbose = getOption("healthdb.verbose"),
  query_only = TRUE,
  ...
)

identify_row(
  data,
  vars,
  match = c("in", "start", "regex", "like", "between", "glue_sql"),
  vals,
  if_all = FALSE,
  ignore_case = TRUE,
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
  [`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).

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

  Matching by "like"/"start" ignores case by default; see `ignore_case`.
  The other match types are unaffected: "in" and "between" compare
  values as they are, and "regex" follows the pattern you supply (use
  `"(?i)"` to make it case-insensitive).

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

- ignore_case:

  A logical for whether `match = "like"` and `"start"` should ignore
  case. Default is TRUE, because codes in administrative data are often
  inconsistently cased, and a case-sensitive match would silently miss
  records. Both the values and the patterns are lower-cased before
  comparison, so the result does not depend on the backend: without it,
  matching would be case-sensitive for data.frames, case-sensitive or
  not on remote tables depending on the database, the database's
  collation, and the 'dbplyr' version. Set to FALSE for a case-sensitive
  match, which on a large table can also be much faster, as wrapping the
  column in `LOWER()` prevents the database from using an index on it.

- verbose:

  A logical for whether to print an explanation of the query and an
  overview of the result. Default is fetching from options. Use
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
identify_rows(iris, starts_with("Sepal"), "between", c(3, 5), if_all = TRUE)
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
#> Warning: memdb_frame(data.frame(...)) was deprecated in dbplyr 2.6.0.
#> ℹ Use `copy_to(memdb(), df)` instead.
identify_rows(iris_db, Species, "like", c("se%", "%ca"))
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL LIKE pattern: se% OR %ca
#> • ignoring case. Use ignore_case = FALSE for a case-sensitive match, which may run faster on a large table because the database can then use an index on the column(s)
#> # A query:  ?? x 5
#> # Database: sqlite 3.53.3 [:memory:]
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

# using glue_sql to write the WHERE clause
# use {`vars`} to refer to the variables selected by vars
# supply additional values required in the query through '...'
# note that if you use LIKE here, you cannot supply multiple patterns in what
identify_rows(iris_db, Species, "glue_sql",
  "{`vars`} LIKE {what}",
  what = "se%"
)
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL WHERE clause: `Species` LIKE 'se%'
#> # A query:  ?? x 5
#> # Database: sqlite 3.53.3 [:memory:]
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
identify_rows(iris_db, Species, "glue_sql",
  "{`vars`} IN ({what*})",
  what = c("setosa", "virginica")
)
#> ℹ Identify records with condition(s):
#> • where the Species column(s) in each record
#> • contains a value satisfied SQL WHERE clause: `Species` IN ('setosa', 'virginica')
#> # A query:  ?? x 5
#> # Database: sqlite 3.53.3 [:memory:]
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
