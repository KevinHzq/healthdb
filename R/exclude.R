#' Remove rows based on conditions or another data set
#'
#' @description
#' This function combines `dplyr::anti_join` and the opposite of `dplyr::filter`. When the second data set is supplied through the `excl` argument, anti join would be performed; otherwise, `data` would be filtered with the expression given via the `condition` argument, and the filter result would in turn be removed using `dplyr::setdiff`.
#'
#' @param data Data frames or remote tables (e.g., from dbplyr). A subset will be removed from this data.
#' @param excl Data frames or remote tables (e.g., from dbplyr). Rows/values present in it will be removed from `data` if there is a match. This will be passed to dplyr::anti_join as the second argument.
#' @param by Column names that should be matched by anti_join or dplyr::join_by expressions. See anti_join's `by` argument for detail. Default NULL is the same as setdiff(data, excl).
#' @param condition An expression that will be passed to dplyr::filter. The rows that satisfy `condition` are those to be removed from `data`.
#' @param verbose A logical for whether printing explanation for the operation. Default is fetching from options. Use options(odcfun.verbose = FALSE) to suppress once and for all.
#' @param report_on A quoted/unquoted column name for counting how many of its distinct values were removed from `data`, e.g., counting how many client IDs were removed. Default is NULL.
#' @param ... Additional arguments passing to filter/anti_join for finer control of matching, e.g., na action, by-group filtering, etc.
#'
#' @return A data frame or remote table that is a subset of `data`.
#' @export
#'
#' @examples
#' # exclude with condition
#' cyl4 <- exclude(mtcars, condition = cyl == 4, report_on = cyl)
#'
#' # exclude with another data
#' exclude(mtcars, cyl4, dplyr::join_by(cyl), report_on = cyl)
exclude <- function(data, excl = NULL, by = NULL, condition = NULL, verbose = getOption("odcfun.verbose"), report_on = NULL, ...) {
  rlang::check_exclusive(excl, condition)
  rlang::check_exclusive(by, condition)

  if (is.null(excl)) {
    # if only data is supplied, do filter then setdiff
    # note that SQL and R handles NA differently
    # this approach mimics R filter(!(condition)) behavior for SQL
    # otherwise, dbplyr translation of filter(!(condition)) returns no NAs
    out_rows <- dplyr::filter(data, {{ condition }}, ...)
    keep_rows <- dplyr::setdiff(data, out_rows)

    if (verbose) cat("\nExclude a subset of `data` that satisfies condition:", deparse(substitute(condition)), ifelse("tbl_sql" %in% class(data), "\nCheck NAs in the result; SQL handles missing value differently compared to R.\n", "\n"))
  } else if (any(dplyr::intersect(class(data), class(excl)) %in% c("data.frame", "tbl_sql"))) {
    # if two data sets, do anti_join
    if (verbose) cat("\nExclude records in `data` through anti_join with `excl` matching on (by argument):", deparse(substitute(by)), "\n")
    keep_rows <- dplyr::anti_join(data, excl, by = {{ by }}, ...)
  } else {
    stop("Both data and excl must be remote tables or local data frames. Try collect() the remote table - may be slow if collecting large data - before runing the function.")
  }

  if (all(!is.null(substitute(report_on)), verbose)) {
    cat_n <- report_n(keep_rows, data, on = {{ report_on }})
    cat("\nOf the", cat_n[2], deparse(substitute(report_on)), "in data,", diff(cat_n), "were excluded.\n")
  }

  return(keep_rows)
}
