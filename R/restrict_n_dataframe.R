#' @export
restrict_n.data.frame <- function(data, clnt_id, n_per_clnt, count_by = NULL, mode = c("flag", "filter"), verbose = getOption("odcfun.verbose")
) {
  mode <- rlang::arg_match0(mode, c("flag", "filter"))

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id_nm <- rlang::as_name(rlang::enquo(clnt_id))

  # place holder for temp var names
  temp_keep_rid <- flag_restrict_n <- NULL

  dt <- data.table::as.data.table(data)

  # count differently if unit id is supplied by count_by
  has_count_by <- !rlang::quo_is_null(rlang::enquo(count_by))
  if (has_count_by) {
    count_by_nm <- rlang::as_name(rlang::enquo(count_by))

    n_filter <- dt[, list(temp_keep_rid = .I[data.table::uniqueN(.SD) >= n_per_clnt]), by = clnt_id_nm, .SDcols = count_by_nm]$temp_keep_rid
  } else {
    n_filter <- dt[, list(temp_keep_rid = .I[.N >= n_per_clnt]), by = clnt_id_nm]$temp_keep_rid
  }

  switch (mode,
    "flag" = dt[, flag_restrict_n := ifelse(.I %in% n_filter, 1, 0)],
    "filter" = {dt <- dt[n_filter]}
  )

  if (verbose) {
    initial_n <- report_n(data, on = {{ clnt_id }})
    cat("\nOf the", initial_n, "clients in the input,", initial_n - report_n(dt, on = {{ clnt_id }}), "were excluded by restricting that each client must have at least", n_per_clnt, "records", ifelse(has_count_by, paste0("with distinct ", count_by_nm), ""), "\n")
  }

  # convert back to dataframe before output
  data.table::setDF(dt)
  return(dt)
}
