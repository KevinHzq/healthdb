#' @export
restrict_n.tbl_sql <- function(data, clnt_id = NULL, n_per_clnt, count_by = NULL, verbose = TRUE) {
  # try to fetch clnt_id from data's attributes
  if (rlang::quo_is_null(rlang::enquo(clnt_id))) {
    if (!is.null(attributes(data)[["clnt_id"]])) {
      clnt_id_nm <- attributes(data)[["clnt_id"]]
    } else {
      stop("`clnt_id` must be supplied if it is not in the data's attributes.")
    }
  } else {
    # as_name(enquo(arg)) converts both quoted and unquoted column name to string
    clnt_id_nm <- rlang::as_name(rlang::enquo(clnt_id))
  }
  # place holder for temp var names
  temp_n_collapsed <- temp_n_collapsed_id <- NULL

  # dt <- data.table::as.data.table(data)

  # count differently if unit id is supplied by count_by
  has_count_by <- !rlang::quo_is_null(rlang::enquo(count_by))

  if (has_count_by) {
    count_by_nm <- rlang::as_name(rlang::enquo(count_by))
    # SQL doesn't have native n_distinct like functions, use dense_rank trick instead
    db <- data %>%
      dplyr::group_by(.data[[clnt_id_nm]]) %>%
      dbplyr::window_order(.data[[count_by_nm]]) %>%
      dplyr::mutate(
        temp_n_collapsed_id = dplyr::dense_rank(.data[[count_by_nm]]),
        temp_n_collapsed = max(temp_n_collapsed_id, na.rm = TRUE)
      ) %>%
      dplyr::filter(temp_n_collapsed >= n_per_clnt) %>%
      dplyr::select(-dplyr::starts_with("temp_"))
  } else {
    db <- data %>%
      dplyr::group_by(.data[[clnt_id_nm]]) %>%
      dplyr::filter(dplyr::n() >= n_per_clnt)
  }

  if (verbose) {
    initial_n <- report_n(data, on = {{ clnt_id }})
    cat("\nOf the", initial_n, "clients in the input,", initial_n - report_n(db, on = {{ clnt_id }}), "were excluded by restricting that each client must have at least", n_per_clnt, "records", ifelse(has_count_by, paste0("with distinct ", count_by_nm), ""), "\n")
  }

  return(dplyr::ungroup(db))
}
