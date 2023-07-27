exclude <- function(data, excl = NULL, by = NULL, condition = NULL, verbose = TRUE, report_on_nm = NULL, ...) {
  if (is.null(excl)) {
    #if only data is supplied, do filter with negation
    if (is.null(substitute(condition))) {
      stop("The `condition` argument must be supplied if `excl` is absent")
    } else {
      if ("data.frame" %in% class(data)) data <- dtplyr::lazy_dt(data)
      keep_rows <- dplyr::filter(data, !({{ condition }}), ...)
      if (verbose) cat("\nExclude a subset of `data` that satisfies condition:", deparse(substitute(condition)), "\n")
    }
  } else if (any(class(data) %in% class(excl))) {
    #if two data sets, do anti_join
    if ("data.frame" %in% class(data)) data <- dtplyr::lazy_dt(data)
    keep_rows <- dplyr::anti_join(data, excl, by = {{ by }}, ...)
    if (verbose) cat("\nExclude records in `data` through anti_join with `excl` matching on (by argument):", deparse(substitute(by)), "\n")
  } else {
    stop("Both incl and excl must be remote tables or local data frames. Try collect() the remote table - may be slow if collecting large data - before runing the function.")
  }

  if (!is.null(report_on_nm)) {
    cat_n <- report_n(keep_rows, data, on_nm = report_on_nm)
    cat("\nOf the", cat_n[2], report_on_nm, "in data,", diff(cat_n), "were excluded.\n")
  }

  if ("dtplyr_step" %in% class(keep_rows)) keep_rows <- as_tibble(keep_rows)
  return(keep_rows)
}
