#' @export
restrict_dates.data.frame <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), flag_at = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("healthdb.verbose"), ...) {
  mode <- rlang::arg_match0(mode, c("flag", "filter"))
  flag_at <- rlang::arg_match0(flag_at, c("left", "right"))
  rlang::check_dots_used()

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # check missing date_var
  check_null <- data %>%
    dplyr::filter(is.na(.data[[date_var]]))
  if (nrow(check_null) > 1) {
    warning("Removed ", nrow(check_null), " records with missing date_var")
    data <- data %>%
      dplyr::setdiff(check_null)
  }

  # place holder for temp var names
  flag_restrict_date <- temp.nm_keep <- temp.nm_keep_cum <- NULL

  if(verbose) {
    initial_n <- report_n(data, on = {{ clnt_id }})
  }

  # see if_dates for detail
  data <- data %>%
    dplyr::group_by(.data[[clnt_id]]) %>%
    dplyr::arrange(.data[[clnt_id]], .data[[date_var]]) %>%
    dplyr::mutate(
      temp.nm_keep = if_dates(.data[[date_var]], n, apart, within, detail = TRUE, flag_at, dup.rm, ...),
      flag_restrict_date = as.numeric(temp.nm_keep)
    )

  n_kept <- data %>%
    dplyr::filter(flag_restrict_date == 1) %>%
    dplyr::n_groups()

  if (mode == "filter") {
    data <- data %>% dplyr::filter(max(flag_restrict_date, na.rm = TRUE) > 0)
  }

  data <- data %>%
    dplyr::select(-dplyr::starts_with("temp.nm_")) %>%
    dplyr::ungroup()

  if (verbose) {
    cat(
      "\n Of the", initial_n, "clients in the input,", initial_n - n_kept, "were", ifelse(mode == "filter", "excluded", "flagged as 0"), "by restricting that each client must have", n, "records that were", ifelse(!is.null(apart), paste("at least", apart, "days apart"), ""), ifelse(!is.null(within), paste("within", within, "days"), "")
      # , ifelse(strict_start, "Records before the earliest entries that met the condition are removed.", "")
      , "\n"
    )
  }

  return(data)
}
