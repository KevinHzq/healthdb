#' @export
restrict_dates.tbl_sql <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), flag_at = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("healthdb.verbose"), check_missing = FALSE, ...) {
  check_con(data)

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

  # check missing date_var; sum an explicit 1L/0L indicator instead of
  # count(is.na(.)) as grouping by a boolean and the row order of the counts
  # are not portable across dialects; as.numeric strips the integer64 class
  # some backends return
  if (check_missing) {
    n_missing <- data %>%
      dplyr::summarise(n_missing = sum(dplyr::if_else(is.na(.data[[date_var]]), 1L, 0L), na.rm = TRUE)) %>%
      dplyr::pull("n_missing") %>%
      as.numeric()
    if (isTRUE(n_missing > 0)) {
      warning("Removed ", n_missing, " records with missing date_var")
      data <- data %>%
        dplyr::filter(!is.na(.data[[date_var]]))
    }
  }
  # place holder for temp var names
  flag_restrict_date <- temp_nm_diff <- temp_nm_gap <- temp_nm_dup <- temp_nm_date <- temp_nm_last_date <- temp_nm_range <- temp_nm_flag_apart <- NULL

  n <- as.integer(n)

  if (is.null(within)) {
    # apart only
    apart <- as.integer(apart)
    data <- data %>%
      dplyr::group_by(.data[[clnt_id]]) %>%
      dbplyr::window_order(.data[[date_var]], .data[[uid]])

    if (is_mssql_mysql) {
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

    if (is_mssql_mysql) {
      data <- data %>%
        dplyr::mutate(
          temp_nm_gap = abs(difftime(.data[[date_var]], temp_nm_diff))
        )
    } else {
      data <- data %>%
        dplyr::mutate(
          temp_nm_gap = abs(temp_nm_diff - .data[[date_var]])
        )
    }

    if (dup.rm) {
      data <- data %>%
        dplyr::mutate(
          # case_when used because the translation for any() failed on SQL server
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
    stopifnot(is.wholenumber(within))
    apart <- as.integer(apart)
    # as.integer, like the within-only branch: date + 30.0 (numeric) is not
    # a valid operation on PostgreSQL, while date + 30 (integer) is
    within <- as.integer(within)

    # same impossibility guard as if_date() so both methods error consistently
    if (apart * (n - 1) > within) stop("Condition is impossible as the n dates would span at least apart*(n - 1) days, which is greater than within")

    if (force_collect) {
      result <- dplyr::collect(data) %>%
        restrict_dates.data.frame(clnt_id = !!clnt_id, date_var = !!date_var, n = n, apart = apart, within = within, flag_at = flag_at, mode = mode, dup.rm = dup.rm, ...)
      return(result)
    } else {
      data <- rlang::try_fetch(
        dplyr::compute(data, temporary = TRUE),
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

    data <- rlang::try_fetch(
      rlang::inject(
        data %>%
          dplyr::left_join(row_date, by = dplyr::join_by(!!rlang::sym(clnt_id), !!rlang::sym(date_var) <= temp_nm_date, temp_nm_range >= temp_nm_date))
      ),
      error = function(cnd) {
        rlang::abort("The attempt of overlap join in SQL failed. Use force_collect = TRUE to download the data before interpreting apart & within condition. Actual error message:\n", parent = cnd)
      }
    )

    data <- data %>%
      dplyr::group_by(.data[[clnt_id]], .data[[uid]]) %>%
      dbplyr::window_order(temp_nm_date)

    if (is_mssql_mysql) {
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
    # report_n not used here to save the extra query execution
    rlang::inform(c("i" = glue::glue('Apply restriction that each client must have {n} records that were{ifelse(!is.null(apart), paste(" at least", apart, "days apart"), "")}{ifelse(!is.null(within), paste(" within", within, "days"), "")}. {ifelse(mode == "filter", "Clients/groups which did not met the condition were excluded.", "Records that met the condition were flagged.")}')))
  }

  return(clean_db(data))
}

all_apart_sqlite <- function(data, date_var, n, apart, clnt_id, uid) {
  # place holder for temp var names
  in_win_1 <- final_win_gap <- NULL

  date_sym <- rlang::sym(date_var)

  # same sliding window from both ends approach as all_apart;
  # window/indicator column names are generated per iteration and injected
  # with !! so user column names can never interfere
  for (i in 1:(n %/% 2)) {
    win_i <- rlang::sym(paste0("in_win_", i))
    win_prev <- rlang::sym(paste0("in_win_", i - 1))
    sum_i <- rlang::sym(paste0("sum_win_", i))

    if ((n - i * 2) == 0) {
      if (n == 2) {
        # case_when, not the bare comparison, as boolean columns cannot be
        # COALESCE'd with 0L or MAX'ed on PostgreSQL, which also takes this
        # branch (its connection class does not match use.datediff())
        data <- data %>%
          dplyr::mutate(temp_nm_flag_apart = dplyr::case_when(
            max(.data[[date_var]], na.rm = TRUE) - min(.data[[date_var]], na.rm = TRUE) >= apart ~ 1L,
            .default = 0L
          ))
        break
      }
      # product of all previous sum_win_* indicators
      sum_prod <- Reduce(
        function(x, y) rlang::expr(!!x * !!y),
        rlang::syms(paste0("sum_win_", 1:(i - 1)))
      )
      data <- data %>%
        dplyr::mutate(
          final_win_gap = max((!!date_sym)[!!win_prev == 1L], na.rm = TRUE) - min((!!date_sym)[!!win_prev == 1L], na.rm = TRUE),
          temp_nm_flag_apart = dplyr::case_when(
            final_win_gap * !!sum_prod >= apart ~ 1L,
            .default = 0L
          )
        )
      break
    } else {
      if (i == 1) {
        data <- data %>%
          dplyr::mutate(
            # case_when makes the indicators integer; PostgreSQL cannot SUM,
            # COALESCE with 0L, or multiply booleans
            in_win_1 = dplyr::case_when(dplyr::between(.data[[date_var]], min(.data[[date_var]], na.rm = TRUE) + apart, max(.data[[date_var]], na.rm = TRUE) - apart) ~ 1L, .default = 0L),
            sum_win_1 = dplyr::case_when(sum(in_win_1, na.rm = TRUE) >= n - i * 2L ~ 1L, .default = 0L)
          )
      } else {
        data <- data %>%
          dplyr::mutate(
            !!win_i := dplyr::case_when(dplyr::between(!!date_sym, min((!!date_sym)[!!win_prev == 1L], na.rm = TRUE) + apart, max((!!date_sym)[!!win_prev == 1L], na.rm = TRUE) - apart) ~ 1L, .default = 0L),
            !!sum_i := dplyr::case_when(sum(!!win_i, na.rm = TRUE) >= n - i * 2L ~ 1L, .default = 0L)
          )
      }
    }
  }

  if (!("temp_nm_flag_apart" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(temp_nm_flag_apart = !!rlang::sym(paste0("sum_win_", i)))
  }

  data <- data %>%
    tidyr::replace_na(list(temp_nm_flag_apart = 0L)) %>%
    dplyr::select(-dplyr::contains("_win_"))
  return(data)
}

all_apart_mssql <- function(data, date_var, n, apart, clnt_id, uid) {
  # place holder for temp var names
  in_win_1 <- final_win_gap <- NULL

  date_sym <- rlang::sym(date_var)

  # same sliding window from both ends approach as all_apart;
  # window/indicator column names are generated per iteration and injected
  # with !! so user column names can never interfere
  for (i in 1:(n %/% 2)) {
    win_i <- rlang::sym(paste0("in_win_", i))
    win_prev <- rlang::sym(paste0("in_win_", i - 1))
    sum_i <- rlang::sym(paste0("sum_win_", i))

    if ((n - i * 2) == 0) {
      if (n == 2) {
        # case_when, not the bare comparison, as boolean columns cannot be
        # COALESCE'd with 0L or MAX'ed on PostgreSQL
        data <- data %>%
          dplyr::mutate(temp_nm_flag_apart = dplyr::case_when(
            abs(difftime(max(.data[[date_var]], na.rm = TRUE), min(.data[[date_var]], na.rm = TRUE), units = "days")) >= apart ~ 1L,
            .default = 0L
          ))
        break
      }
      # product of all previous sum_win_* indicators
      sum_prod <- Reduce(
        function(x, y) rlang::expr(!!x * !!y),
        rlang::syms(paste0("sum_win_", 1:(i - 1)))
      )
      data <- data %>%
        dplyr::mutate(
          final_win_gap = abs(difftime(max((!!date_sym)[!!win_prev == 1L], na.rm = TRUE), min((!!date_sym)[!!win_prev == 1L], na.rm = TRUE), units = "days")),
          temp_nm_flag_apart = dplyr::case_when(final_win_gap * !!sum_prod >= apart ~ 1L, .default = 0L)
        )
      break
    } else {
      if (i == 1) {
        data <- data %>%
          dplyr::mutate(
            # case_when makes the indicator integer (PostgreSQL cannot SUM
            # booleans); explicit comparisons, not between(), as SQL Server
            # translates between() to a bit-valued IIF, which cannot be used
            # as a CASE WHEN condition
            in_win_1 = dplyr::case_when(.data[[date_var]] >= clock::add_days(min(.data[[date_var]], na.rm = TRUE), apart) & .data[[date_var]] <= clock::add_days(max(.data[[date_var]], na.rm = TRUE), -apart) ~ 1L, .default = 0L),
            sum_win_1 = dplyr::case_when(sum(in_win_1, na.rm = TRUE) >= local(as.integer(n - i * 2)) ~ 1L,
              .default = 0L
            )
          )
      } else {
        data <- data %>%
          dplyr::mutate(
            !!win_i := dplyr::case_when(!!date_sym >= clock::add_days(min((!!date_sym)[!!win_prev == 1L], na.rm = TRUE), apart) & !!date_sym <= clock::add_days(max((!!date_sym)[!!win_prev == 1L], na.rm = TRUE), -apart) ~ 1L, .default = 0L),
            !!sum_i := dplyr::case_when(sum(!!win_i, na.rm = TRUE) >= local(as.integer(n - i * 2)) ~ 1L,
              .default = 0L
            )
          )
      }
    }
  }

  if (!("temp_nm_flag_apart" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(temp_nm_flag_apart = !!rlang::sym(paste0("sum_win_", i)))
  }

  data <- data %>%
    tidyr::replace_na(list(temp_nm_flag_apart = 0L)) %>%
    dplyr::select(-dplyr::contains("_win_"))
  return(data)
}

use.datediff <- function(data) {
  stringr::str_detect(stringr::str_to_lower(dbplyr::remote_con(data) %>% class()), "sql server|redshift|snowflake|postgres") %>% any()
}
