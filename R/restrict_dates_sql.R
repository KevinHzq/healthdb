#' @export
restrict_dates.tbl_sql <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), flag_at = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("healthdb.verbose"), ...) {
  stopifnot(n > 1, is.wholenumber(n))

  mode <- rlang::arg_match0(mode, c("flag", "filter"))
  flag_at <- rlang::arg_match0(flag_at, c("left", "right"))
  rlang::check_dots_used()

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # check missing date_var
  check_null <- data %>%
    dplyr::count(no_date = is.na(.data[[date_var]])) %>%
    dplyr::collect()
  if (nrow(check_null) > 1) {
    warning("Removed ", check_null[2, 2], " records with missing date_var")
    data <- data %>%
      dplyr::filter(!is.na(.data[[date_var]]))
  }

  # place holder for temp var names
  flag_restrict_date <- temp_nm_keep_max <- temp_nm_keep_cum <- temp_nm_diff <- temp_nm_gap <- temp_nm_dup <- temp_nm_last_date <- NULL

  n <- as.integer(n)

  if (!is.null(apart)) {
    if (!force_collect) {
      stop("`apart` is implemented for local data frame only. Use force_collect = TRUE argument to proceed with downloading the remote table (may be slow)")
    } else {
      # see if_dates for detail
      keep <- dplyr::collect(data) %>%
        restrict_dates.data.frame(clnt_id = !!clnt_id, date_var = !!date_var, n = n, apart = apart, within = within, flag_at = flag_at, mode = mode, dup.rm = dup.rm, ...)
    }
  } else {
    has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
    if (!has_uid) {
      stop("`uid` must be supplied for database input to produce deterministic result")
    } else {
      uid <- rlang::as_name(rlang::enquo(uid))
    }

    # same logic as rollapply for within only in if_dates
    stopifnot(is.wholenumber(within))
    within <- as.integer(within)

    # to get consecutive unique dates, flag duplicate and use it to sort
    if (dup.rm) {
      data <- data %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]]) %>%
        dplyr::mutate(
          temp_nm_last_date = dplyr::lag(.data[[date_var]]),
          temp_nm_dup = dplyr::case_when(
            .data[[date_var]] == temp_nm_last_date ~ 1L,
            is.na(temp_nm_last_date) ~ 0L,
            .default = 0L
          )
        )
      keep <- data %>%
        dplyr::group_by(.data[[clnt_id]], temp_nm_dup) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]])
    } else {
      keep <- data %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]])
    }
    # browser()

    switch(flag_at,
      left = {
        keep <- keep %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lead(.data[[date_var]], n = local(n - 1L))
          )
      },
      right = {
        keep <- keep %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lag(.data[[date_var]], n = local(n - 1L))
          )
      }
    )
    # browser()

    # SQL server does not accept subtracting dates
    is_mssql_mysql <- stringr::str_detect(dbplyr::remote_con(data) %>% class(), "SQL Server|Maria") %>% any()

    if (is_mssql_mysql) {
      keep <- keep %>%
        dplyr::mutate(
          temp_nm_gap = dbplyr::sql(glue::glue_sql("ABS(DATEDIFF(day, {`date_var`}, {`temp_nm_diff`}))", .con = dbplyr::remote_con(data), temp_nm_diff = "temp_nm_diff"))
        )
    } else {
      keep <- keep %>%
        dplyr::mutate(
          temp_nm_gap = abs(temp_nm_diff - .data[[date_var]])
        )
    }
    # browser()

    if (dup.rm) {
      keep <- keep %>%
        dplyr::mutate(
          # the translation for any() failed on SQL server again
          # temp_nm_keep = any(temp_nm_gap <= within, na.rm = TRUE)
          flag_restrict_date = dplyr::case_when(
            temp_nm_dup == 1L ~ NA,
            temp_nm_gap <= within ~ 1L,
            is.na(temp_nm_gap) ~ 0L,
            .default = 0L
          )
        )
      # sort back to original order
      keep <- keep %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]]) %>%
        tidyr::fill(flag_restrict_date, .direction = "down")

    } else {
      keep <- keep %>%
        dplyr::mutate(
          flag_restrict_date = dplyr::case_when(
            temp_nm_gap <= within ~ 1L,
            is.na(temp_nm_gap) ~ 0L,
            .default = 0L
          )
        )
    }

    if (mode == "filter") {
      keep <- keep %>% dplyr::filter(max(flag_restrict_date, na.rm = TRUE) > 0L)
    }

    keep <- keep %>%
      dplyr::select(-dplyr::starts_with("temp_")) %>%
      dplyr::ungroup()
  }

  if (verbose) {
    # disable report_n to save the extra execution
    # initial_n <- report_n(data, on = {{ clnt_id }})
    # cat("\nOf the", initial_n, "clients in the input,", initial_n - report_n(keep, on = {{ clnt_id }}), "were excluded by restricting that each client must have", n, "records that were", ifelse(!is.null(apart), paste("at least", apart, "days apart"), ""), "within", within, "days.\n")
    cat("\nApply restriction that each client must have", n, "records that were", ifelse(!is.null(apart), paste("at least", apart, "days apart"), ""), "within", within, "days.", ifelse(mode == "filter", "Clients/groups which did not met the condition were excluded", "Records that met the condition were flagged."), "\n")
  }

  return(keep)
}
