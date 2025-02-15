#' @export
restrict_dates.tbl_sql <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), flag_at = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("healthdb.verbose"), check_missing = FALSE, ...) {
  mode <- rlang::arg_match0(mode, c("flag", "filter"))
  flag_at <- rlang::arg_match0(flag_at, c("left", "right"))
  rlang::check_dots_used()

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  date_var <- rlang::as_name(rlang::enquo(date_var))

  # SQL server does not accept subtracting dates
  is_mssql_mysql <- use.datediff(data)

  has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
  if (!has_uid) {
    stop("`uid` must be supplied for database input to produce deterministic result")
  } else {
    uid <- rlang::as_name(rlang::enquo(uid))
  }

  # check missing date_var
  if (check_missing) {
    check_null <- data %>%
      dplyr::count(no_date = is.na(.data[[date_var]])) %>%
      dplyr::collect()
    if (nrow(check_null) > 1) {
      warning("Removed ", check_null[2, 2], " records with missing date_var")
      data <- data %>%
        dplyr::filter(!is.na(.data[[date_var]]))
    }
  }
  # place holder for temp var names
  flag_restrict_date <- temp_nm_diff <- temp_nm_gap <- temp_nm_dup <- temp_nm_date <- temp_nm_last_date <- temp_nm_range <- temp_nm_flag_apart <- NULL

  n <- as.integer(n)

  if (is.null(within)) {
    # original code without apart for tbl_sql
    # if (!force_collect) {
    #   stop("`apart` is implemented for local data frame only. Use force_collect = TRUE argument to proceed with downloading the remote table (may be slow)")
    # } else {
    #   # see if_dates for detail
    #   keep <- dplyr::collect(data) %>%
    #     restrict_dates.data.frame(clnt_id = !!clnt_id, date_var = !!date_var, n = n, apart = apart, within = within, flag_at = flag_at, mode = mode, dup.rm = dup.rm, ...)
    # }

    # apart only
    apart <- as.integer(apart)
    data <- data %>%
      dplyr::group_by(.data[[clnt_id]]) %>%
      dbplyr::window_order(.data[[date_var]], .data[[uid]])

    if(is_mssql_mysql) {
      data <- all_apart_mssql(data, date_var, n, apart, clnt_id, uid)
    } else {
      data <- all_apart_sqlite(data, date_var, n, apart, clnt_id, uid)
    }

    data <- data %>%
      dplyr::mutate(flag_restrict_date = temp_nm_flag_apart)
  } else if (is.null(apart)) {
    # within only
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
      data <- data %>%
        dplyr::group_by(.data[[clnt_id]], temp_nm_dup) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]])
    } else {
      data <- data %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]])
    }
    # browser()

    switch(flag_at,
      left = {
        data <- data %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lead(.data[[date_var]], n = local(n - 1L))
          )
      },
      right = {
        data <- data %>%
          dplyr::mutate(
            temp_nm_diff = dplyr::lag(.data[[date_var]], n = local(n - 1L))
          )
      }
    )
    # browser()

    if (is_mssql_mysql) {
      data <- data %>%
        dplyr::mutate(
          # temp_nm_gap = dbplyr::sql(glue::glue_sql("ABS(DATEDIFF(day, {`date_var`}, {`temp_nm_diff`}))", .con = dbplyr::remote_con(data), temp_nm_diff = "temp_nm_diff"))
          temp_nm_gap = abs(difftime(.data[[date_var]], temp_nm_diff))
        )
    } else {
      data <- data %>%
        dplyr::mutate(
          temp_nm_gap = abs(temp_nm_diff - .data[[date_var]])
        )
    }
    # browser()

    if (dup.rm) {
      data <- data %>%
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
      data <- data %>%
        dplyr::group_by(.data[[clnt_id]]) %>%
        dbplyr::window_order(.data[[date_var]], .data[[uid]]) %>%
        tidyr::fill(flag_restrict_date, .direction = "down")
    } else {
      data <- data %>%
        dplyr::mutate(
          flag_restrict_date = dplyr::case_when(
            temp_nm_gap <= within ~ 1L,
            is.na(temp_nm_gap) ~ 0L,
            .default = 0L
          )
        )
    }

    data <- data %>%
      tidyr::replace_na(list(flag_restrict_date = 0L))
  } else {
    # apart and within
    # do overlap join then all_apart
    apart <- as.integer(apart)

    if (force_collect) {
      result <- dplyr::collect(data) %>%
        restrict_dates.data.frame(clnt_id = !!clnt_id, date_var = !!date_var, n = n, apart = apart, within = within, flag_at = flag_at, mode = mode, dup.rm = dup.rm, ...)
      return(result)
    } else {
      data <- rlang::try_fetch(
        dplyr::compute(data,
          # maybe not worth it to create index
          # indexes = list(clnt_id, date_var),
          temporary = TRUE
        ),
        error = function(cnd) {
          rlang::warn("The attempt to write a temp table for performance boost failed. Actual error message:\n", parent = cnd)
          return(data)
        }
      )
    }

    nm_lu <- c(temp_nm_date = date_var)

    row_date <- data %>%
      dplyr::select(dplyr::all_of(c(clnt_id, date_var))) %>%
      dplyr::rename(dplyr::all_of(nm_lu)) %>%
      # here may give a warning as an order by is added somehow
      # try to fix
      dplyr::arrange() %>%
      dplyr::collapse()

    if (is_mssql_mysql) {
      data <- data %>%
        dplyr::mutate(temp_nm_range = clock::add_days(.data[[date_var]], within))
    } else {
      data <- data %>%
        dplyr::mutate(temp_nm_range = .data[[date_var]] + within)
    }

    expr_overlap <- rlang::expr({
      data %>%
        dplyr::left_join(row_date, by = dplyr::join_by(clnt_id, date_var <= temp_nm_date, temp_nm_range >= temp_nm_date))
    })

    data <- rlang::expr_text(expr_overlap) %>%
      stringr::str_replace_all("clnt_id", glue::glue("{clnt_id}")) %>%
      stringr::str_replace_all("date_var", glue::glue("{date_var}")) %>%
      rlang::parse_expr() %>%
      eval()

    data <- rlang::try_fetch(
      data,
      error = function(cnd) {
        rlang::abort("The attempt of overlap join in SQL failed. Use force_collect = TRUE to download the data before interpreting apart & within condition. Actual error message:\n", parent = cnd)
      }
    )

    data <- data %>%
      dplyr::group_by(.data[[clnt_id]], .data[[uid]]) %>%
      dbplyr::window_order(temp_nm_date)

    if(is_mssql_mysql) {
      data <- all_apart_mssql(data, "temp_nm_date", n, apart, clnt_id, uid)
    } else {
      data <- all_apart_sqlite(data, "temp_nm_date", n, apart, clnt_id, uid)
    }

    data <- data %>%
      dplyr::slice_min(.data[[uid]], n = 1, with_ties = FALSE) %>%
      dplyr::mutate(flag_restrict_date = temp_nm_flag_apart)
  }

  if (mode == "filter") {
    data <- data %>%
      dplyr::group_by(.data[[clnt_id]]) %>%
      dbplyr::window_order(.data[[uid]]) %>%
      dplyr::filter(max(flag_restrict_date, na.rm = TRUE) > 0L)
  }

  data <- data %>%
    dplyr::select(-dplyr::starts_with("temp_nm_"))


  if (verbose) {
    # disable report_n to save the extra execution
    # initial_n <- report_n(data, on = {{ clnt_id }})
    rlang::inform(c("i" = glue::glue('Apply restriction that each client must have {n} records that were{ifelse(!is.null(apart), paste(" at least", apart, "days apart"), "")}{ifelse(!is.null(within), paste(" within", within, "days"), "")}. {ifelse(mode == "filter", "Clients/groups which did not met the condition were excluded.", "Records that met the condition were flagged.")}')))
  }

  return(clean_db(data))
}

all_apart_sqlite <- function(data, date_var, n, apart, clnt_id, uid) {
  in_win_1 <- in_win_i <- in_win_x <- final_win_gap <- NULL
  # browser()
  # same sliding window from both ends approach as all_apart
  for (i in 1:(n %/% 2)) {
    if ((n - i * 2) == 0) {
      if (n == 2) {
        data <- data %>%
          dplyr::mutate(temp_nm_flag_apart = max(.data[[date_var]], na.rm = TRUE) - min(.data[[date_var]], na.rm = TRUE) >= apart)
        break
      }
      expr_flag <- rlang::expr({
        data %>%
          dplyr::mutate(
            final_win_gap = max(date_var[in_win_x == 1L], na.rm = TRUE) - min(date_var[in_win_x == 1L], na.rm = TRUE),
            temp_nm_flag_apart = final_win_gap * !!rlang::parse_expr(paste0("sum_win_", 1:(i - 1), collapse = " * ")) >= apart
          )
      })
      data <- rlang::expr_text(expr_flag) %>%
        stringr::str_replace_all("_x", glue::glue("_{i - 1}")) %>%
        stringr::str_replace_all("date_var", glue::glue("{date_var}")) %>%
        rlang::parse_expr() %>%
        eval()
      # browser()
      break
    } else {
      if (i == 1) {
        data <- data %>%
          dplyr::mutate(
            in_win_1 = dplyr::between(.data[[date_var]], min(.data[[date_var]], na.rm = TRUE) + apart, max(.data[[date_var]], na.rm = TRUE) - apart),
            sum_win_1 = sum(in_win_1, na.rm = TRUE) >= n - i * 2L
          )
      } else {
        expr_win_i <- rlang::expr({
          data %>%
            dplyr::mutate(
              in_win_i = dplyr::between(date_var, min(date_var[in_win_x == 1L], na.rm = TRUE) + apart, max(date_var[in_win_x == 1L], na.rm = TRUE) - apart),
              sum_win_i = sum(in_win_i, na.rm = TRUE) >= n - i * 2L
            )
        })
        data <- rlang::expr_text(expr_win_i) %>%
          stringr::str_replace_all("_i", glue::glue("_{i}")) %>%
          stringr::str_replace_all("_x", glue::glue("_{i - 1}")) %>%
          stringr::str_replace_all("date_var", glue::glue("{date_var}")) %>%
          rlang::parse_expr() %>%
          eval()
      }
    }
  }

  if (!("temp_nm_flag_apart" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(temp_nm_flag_apart = !!rlang::parse_expr(paste0("sum_win_", i)))
  }

  data <- data %>%
    tidyr::replace_na(list(temp_nm_flag_apart = 0L)) %>%
    dplyr::select(-dplyr::contains("_win_"))
  return(data)
}

all_apart_mssql <- function(data, date_var, n, apart, clnt_id, uid) {
  # browser()
  # same sliding window from both ends approach as all_apart
  in_win_x <- NULL
  for (i in 1:(n %/% 2)) {
    if ((n - i * 2) == 0) {
      if (n == 2) {
        data <- data %>%
          dplyr::mutate(temp_nm_flag_apart = difftime(min(.data[[date_var]], na.rm = TRUE), max(.data[[date_var]], na.rm = TRUE), units = "days") >= apart)
        break
      }
      expr_flag <- rlang::expr({
        data %>%
          dplyr::mutate(
            final_win_gap = difftime(min(date_var[in_win_x == 1L], na.rm = TRUE), max(date_var[in_win_x == 1L], na.rm = TRUE), units = "days"),
            temp_nm_flag_apart = dplyr::case_when(final_win_gap * !!rlang::parse_expr(paste0("sum_win_", 1:(i - 1), collapse = " * ")) >= apart ~ 1L, .default = 0L)
          )
      })
      data <- rlang::expr_text(expr_flag) %>%
        stringr::str_replace_all("_x", glue::glue("_{i - 1}")) %>%
        stringr::str_replace_all("date_var", glue::glue("{date_var}")) %>%
        rlang::parse_expr() %>%
        eval()
      # browser()
      break
    } else {
      if (i == 1) {
        data <- data %>%
          dplyr::mutate(
            in_win_1 = dplyr::between(.data[[date_var]], clock::add_days(min(.data[[date_var]], na.rm = TRUE), apart), clock::add_days(max(.data[[date_var]], na.rm = TRUE), -apart)),
            sum_win_1 = dplyr::case_when(sum(in_win_1, na.rm = TRUE) >= local(as.integer(n - i * 2)) ~ 1L,
                                          .default = 0L)
          )
      } else {
        expr_win_i <- rlang::expr({
          data %>%
            dplyr::mutate(
              in_win_i = dplyr::between(date_var, clock::add_days(min(date_var[in_win_x == 1L], na.rm = TRUE), apart), clock::add_days(max(date_var[in_win_x == 1L], na.rm = TRUE), -apart)),
              sum_win_i = dplyr::case_when(sum(in_win_i, na.rm = TRUE) >= local(as.integer(n - i * 2)) ~ 1L,
                                            .default = 0L)
            )
        })
        data <- rlang::expr_text(expr_win_i) %>%
          stringr::str_replace_all("_i", glue::glue("_{i}")) %>%
          stringr::str_replace_all("_x", glue::glue("_{i - 1}")) %>%
          stringr::str_replace_all("date_var", glue::glue("{date_var}")) %>%
          rlang::parse_expr() %>%
          eval()
      }
    }
  }

  if (!("temp_nm_flag_apart" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(temp_nm_flag_apart = !!rlang::parse_expr(paste0("sum_win_", i)))
  }

  data <- data %>%
    tidyr::replace_na(list(temp_nm_flag_apart = 0L)) %>%
    dplyr::select(-dplyr::contains("_win_"))
  return(data)
}

use.datediff <- function(data) {
  stringr::str_detect(stringr::str_to_lower(dbplyr::remote_con(data) %>% class()), "sql server|redshift|snowflake|postgres") %>% any()
}
