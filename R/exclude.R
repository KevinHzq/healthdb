exclude <- function(data, excl = NULL, by = NULL, condition = NULL, verbose = TRUE, report_on = NULL, ...) {
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
    if (verbose) cat("\nExclude records in `data` through anti_join with `excl` matching on (by argument):", deparse(substitute(by)), "\n")
    keep_rows <- dplyr::anti_join(data, excl, by = {{ by }}, ...)
  } else {
    stop("Both data and excl must be remote tables or local data frames. Try collect() the remote table - may be slow if collecting large data - before runing the function.")
  }

  if (!is.null(substitute(report_on))) {
    cat_n <- report_n(keep_rows, data, on = {{ report_on }} )
    cat("\nOf the", cat_n[2], deparse(substitute(report_on)), "in data,", diff(cat_n), "were excluded.\n")
  }

  if ("dtplyr_step" %in% class(keep_rows)) keep_rows <- dplyr::as_tibble(keep_rows)
  return(keep_rows)
}
