#' @export
restrict_n.tbl_sql <- function(data, clnt_id, n_per_clnt, count_by = NULL, mode = c("flag", "filter"), verbose = getOption("healthdb.verbose")) {
  check_con(data)

  mode <- rlang::arg_match0(mode, c("flag", "filter"))

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id_nm <- rlang::as_name(rlang::enquo(clnt_id))

  # place holder for temp var names
  temp_n_collapsed <- flag_restrict_n <- temp_n_collapsed_id <- NULL

  # count differently if unit id is supplied by count_by
  has_count_by <- !rlang::quo_is_null(rlang::enquo(count_by))

  n_per_clnt <- as.integer(n_per_clnt)

  if (has_count_by) {
    count_by_nm <- rlang::as_name(rlang::enquo(count_by))
    # SQL doesn't have native n_distinct like functions, use dense_rank trick instead
    db <- data %>%
      dplyr::group_by(.data[[clnt_id_nm]]) %>%
      dbplyr::window_order(.data[[count_by_nm]]) %>%
      dplyr::mutate(
        temp_n_collapsed_id = dplyr::dense_rank(.data[[count_by_nm]]),
        temp_n_collapsed = max(temp_n_collapsed_id, na.rm = TRUE),
        flag_restrict_n = ifelse(temp_n_collapsed >= n_per_clnt, 1L, 0L)
      )

  } else {
    db <- data %>%
      dplyr::group_by(.data[[clnt_id_nm]]) %>%
      dplyr::mutate(
        temp_n_collapsed = dplyr::n(),
        flag_restrict_n = ifelse(temp_n_collapsed >= n_per_clnt, 1L, 0L)
      )
  }

  db <- db %>% dplyr::select(-dplyr::starts_with("temp_"))

  if (mode == "filter") {
    db <- db %>% dplyr::filter(flag_restrict_n == 1L)
  }

  if (verbose) {
    # disable report_n to save the extra execution
    # initial_n <- report_n(data, on = {{ clnt_id }})
    # cat("\nApply restriction that each client must have at least", n_per_clnt, paste0(ifelse(has_count_by, paste0("records with distinct ", count_by_nm, "."), "records."), " Clients/groups which", ifelse(mode == "filter", "did not meet", "met"), "the condition were"), ifelse(mode == "filter", "excluded.", "flagged."), "\n")
    rlang::inform(c("i" = glue::glue('Apply restriction that each client must have at least {n_per_clnt} {ifelse(has_count_by, paste0("records with distinct ", count_by_nm), "records")}. {ifelse(mode == "filter", "Clients/groups which did not met the condition were excluded.", "Records that met the condition were flagged.")}')))
  }

  return(clean_db(db))
}
