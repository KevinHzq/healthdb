#' @export
restrict_dates.tbl_sql <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, dup.rm = TRUE, force_collect = FALSE, verbose = TRUE, ...) {
  stopifnot(n > 1, is.wholenumber(n))

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # place holder for temp var names
  temp.nm_keep <- temp.nm_lead <- temp.nm_gap <- NULL

  if (!is.null(apart)) {
    if (!force_collect) {
      stop("`apart` is implemented for local data frame only. Use force_collect = TRUE argument to proceed with downloading the remote table (may be slow)")
    } else {
      # see if_dates for detail
      keep <- dplyr::collect(data) %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dplyr::arrange(.data[[clnt_id]], .data[[date_var]]) %>%
        dplyr::mutate(temp.nm_keep = if_dates(.data[[date_var]], n, apart, within, dup.rm, ...)) %>%
        dplyr::filter(temp.nm_keep) %>%
        dplyr::select(-dplyr::starts_with("temp.nm_")) %>%
        dplyr::ungroup()
    }
  } else {
    # same logic as rollapply for within only in if_dates
    stopifnot(is.wholenumber(within))
    keep <- data %>%
      dplyr::group_by(.data[[clnt_id]]) %>%
      dbplyr::window_order(.data[[date_var]]) %>%
      dplyr::mutate(
        temp.nm_lead = dplyr::lead(.data[[date_var]], n - 1),
        temp.nm_gap = temp.nm_lead - .data[[date_var]],
        temp.nm_keep = any(temp.nm_gap <= within, na.rm = TRUE)
      ) %>%
      dplyr::filter(temp.nm_keep) %>%
      dplyr::select(-dplyr::starts_with("temp.nm_")) %>%
      dplyr::ungroup()
  }

  if (verbose) {
    initial_n <- report_n(data, on = {{ clnt_id }})
    cat("\nOf the", initial_n, "clients in the input,", initial_n - report_n(keep, on = {{ clnt_id }}), "were excluded by restricting that each client must have", n, "records that were", ifelse(!is.null(apart), paste("at least", apart, "days apart"), ""), "within", within, "days.\n")
  }

  return(keep)
}
