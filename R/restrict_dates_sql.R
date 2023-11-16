#' @export
restrict_dates.tbl_sql <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), align = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("odcfun.verbose"), ...) {
  stopifnot(n > 1, is.wholenumber(n))

  mode <- rlang::arg_match0(mode, c("flag", "filter"))
  align <- rlang::arg_match0(align, c("left", "right"))

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # place holder for temp var names
  flag_restrict_dates <- temp_nm_keep_max <- temp_nm_keep_cum <- temp_nm_diff <- temp_nm_gap <- temp_nm_drank <- temp_nm_drank_diff <- temp_nm_drank_gap <- NULL

  n <- as.integer(n)

  if (!is.null(apart)) {
    if (!force_collect) {
      stop("`apart` is implemented for local data frame only. Use force_collect = TRUE argument to proceed with downloading the remote table (may be slow)")
    } else {
      # see if_dates for detail
      keep <- dplyr::collect(data) %>%
        restrict_dates.data.frame(clnt_id = !!clnt_id, date_var = !!date_var, n = n, apart = apart, within = within, align = align, mode = mode, dup.rm = dup.rm, ...)
      # dplyr::group_by(.data[[clnt_id]]) %>%
      # dplyr::arrange(.data[[clnt_id]], .data[[date_var]]) %>%
      # dplyr::mutate(temp_nm_keep = if_dates(.data[[date_var]], n, apart, within, dup.rm, ...)) %>%
      # dplyr::filter(temp_nm_keep) %>%
      # dplyr::select(-dplyr::starts_with("temp_")) %>%
      # dplyr::ungroup()
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

    keep <- data %>%
      dplyr::group_by(.data[[clnt_id]]) %>%
      dbplyr::window_order(.data[[date_var]], .data[[uid]]) %>%
      dplyr::mutate(
        temp_nm_drank = dplyr::dense_rank(.data[[date_var]])
        )
    #browser()

    switch (align,
      left = {
        keep <- keep %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lead(.data[[date_var]], n = local(n - 1L)),
            temp_nm_drank_diff = dplyr::lead(temp_nm_drank, n = local(n - 1L))
          )
      },
      right = {
        keep <- keep %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lag(.data[[date_var]], n = local(n - 1L)),
            temp_nm_drank_diff = dplyr::lag(temp_nm_drank, n = local(n - 1L))
          )
      }
    )
    #browser()

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
    #browser()

    keep <- keep %>%
      dplyr::mutate(
        # the translation for any() failed on SQL server again
        # temp_nm_keep = any(temp_nm_gap <= within, na.rm = TRUE)
        temp_nm_drank_gap = abs(temp_nm_drank_diff - temp_nm_drank),
        flag_restrict_dates = dplyr::case_when(
          temp_nm_gap == 0L ~ 0L,
          temp_nm_gap <= within & temp_nm_drank_gap == n - 1L ~ 1L,
          is.na(temp_nm_gap) ~ 0L,
          .default = 0L
        )
      )

    # if (strict_start) {
    #   keep <- keep %>%
    #     dplyr::mutate(temp_nm_keep_cum = cummax(temp_nm_keep))
    #   switch(mode,
    #     "flag" = {
    #       keep <- keep %>%
    #         dplyr::mutate(flag_restrict_dates = temp_nm_keep_cum)
    #     },
    #     "filter" = {
    #       keep <- keep %>% dplyr::filter(temp_nm_keep_cum > 0L)
    #     }
    #   )
    # } else {
    #   keep <- keep %>%
    #     dplyr::mutate(temp_nm_keep_max = max(temp_nm_keep, na.rm = TRUE))
    #   switch(mode,
    #     "flag" = {
    #       keep <- keep %>%
    #         dplyr::mutate(flag_restrict_dates = temp_nm_keep_max)
    #     },
    #     "filter" = {
    #       keep <- keep %>% dplyr::filter(temp_nm_keep_max >= 1L)
    #     }
    #   )
    # }
    #browser()

    if (mode == "filter") {
      keep <- keep %>% dplyr::filter(max(flag_restrict_dates, na.rm = TRUE) > 0L)
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
