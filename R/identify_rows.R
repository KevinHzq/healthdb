#' Identify rows with a match
#'
#' @md
#' @description
#' Filter rows which values satisfy the specified conditions. The function is similar to dplyr::filter(if_any/if_all(...)), but it used `data.table` for data.frame method, and has regular regular expression support for remote database tables. The motivation is to take away some pain when working with databases which often have no regex and LIKE with multiple patterns support.
#'
#' @param data Data frames or remote tables (e.g., from dbplyr)
#' @param vars An expression passing to `dplyr::select()`. It can be Quoted/unquoted column names, or `tidyselect` helper functions, such as `starts_with()`.
#' @param match One of "in", "start", "regex", "like", "between", and "glue_sql". It determines how values would be matched. The operations under each type:
#'  - "in" ~ var %in% vals
#'  - "regex" ~ var data.table::`%like%` vals. For remote tables, unique values in vars are collected locally before matching (may be slow).
#'  - "like" ~ stringr::str_like(var, vals). For remote tables, WHERE var LIKE val.
#'  - "start" ~ same as regex or LIKE with modified vals, e.g., "^val1|^val2" or "va1%|val2%"
#'  - "between" ~ dplyr::between(var, val1, val2)
#'  - "glue_sql" ~ For remote table only, this gives full control of the WHERE clause using dplyr::filter(dbplyr::sql(glue::glue_sql(...)))
#' @param vals Depending on `match`, it takes different input:
#'  - "in" ~ a vector of values (numeric/character/Date)
#'  - "start" ~ a vector of numeric/character that would be modified into a regex or LIKE pattern string by adding "^" in front or "%" at the end
#'  - "regex"/"like" ~ a string of the expression
#'  - "between" ~ a vector of numeric or date with exactly two elements, e.g., c(lower, upper)
#'  - "glue_sql" ~ a string of the part of query after WHERE, which will be passed to glue::glue_sql(). See ?glue_sql and example for detail.
#' @param if_all A logical for whether combining the predicates (if multiple columns were selected by vars) with AND instead of OR. Default is FALSE, e.g., var1 in vals OR var2 in vals.
#' @param verbose A logical for whether printing explanation and result overview for the query.
#' @param query_only A logical for whether keeping the output as remote table (Default TRUE) or downloading the query result as a tibble (FALSE). The argument is ignored when the input data is a data.frame/tibble.
#' @param ... For remote table method only. Additional arguments passing to `glue::glue_sql()` for parameterized queries.
#'
#' @return A data.frame or dplyr::tbl() object depending on the input.
#' @export
#'
#' @examples
#' #applying to data.frame; both sepal length and width in range 3-5
#' identify_rows(iris, starts_with("Sepal"), "between", c(3, 5), if_all = TRUE)
#'
#' #applying to remote table; species starts with se or ends with ca
#' iris_db <- dbplyr::memdb_frame(iris)
#' identify_rows(iris_db, Species, "like", c("se%", "%ca"))
#'
#' #using glue_sql to write the WHERE clause
#' #use {`vars`} to refer to the variables selected by vars
#' #supply additional values required in the query through the ...
#' #note that if you use LIKE here, you cannot supply multiple patterns in what
#' identify_rows(iris_db, Species, "glue_sql", "{`vars`} LIKE {what}", what = "se%")
#'
#' #you could also set query parameters in the global environment
#' what <- c("setosa", "virginica")
#' identify_rows(iris_db, Species, "glue_sql", "{`vars`} IN ({what*})")
identify_rows <- function(data, vars, match = c("in", "start", "regex", "like", "between", "glue_sql"), vals, if_all = FALSE, verbose = TRUE, query_only = TRUE, ...) {
  rlang::check_required(vars)
  rlang::check_required(vals)
  UseMethod("identify_rows")
}
