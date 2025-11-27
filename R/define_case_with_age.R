#' Identify diseases/events from administrative records with age restriction
#'
#' @md
#' @description
#' This function extends the standard case definition function [define_case()] by allowing age-based filtering. See [define_case()] for more general description of what this function does.
#'
#' Note that when using this function with an existing age variable, the age should be determined at the time of the record. Records that are not in the eligible age range will be remove before interpreting the temporal relationship between records. In other words, the age restriction is applied before [restrict_date()].
#'
#' For other age restrictions based on a fixed time point (e.g., age at the baseline of follow-up), it can be done by filtering the input data or output of [define_case()] instead of using this function.
#'
#' @inheritParams identify_row
#' @param match One of "in", "start", "regex", "like", "between", and "glue_sql". It determines how values would be matched. See [identify_row()] for detail.
#' @param vals Depending on `match`, it takes different input. See [identify_row()].
#' @inheritParams restrict_n
#' @inheritParams restrict_date
#' @param excl_vals Same as `vals` but clients/groups with these values are going to be removed from the result. This is intended for exclusion criteria of a case definition.
#' @param excl_args A named list of arguments passing to the second [identify_row()] call for `excl_vals`. If not supplied, `var`, `match` and `if_all` of the first call will be re-used.
#' @param keep One of:
#' * "first" (keeping each client's earliest record),
#' * "last" (keeping the latest),
#' * and "all" (keeping all relevant records, default).
#' * Note that "first"/"last" should not be used with "flag" mode.
#' @param mode Either:
#' * "flag" - add new columns starting with "flag_" indicating if the client met the condition,
#' * or "filter" - remove clients that did not meet the condition from the data.
#' * This will be passed to both [restrict_n()] AND [restrict_date()]. Default is "flag".
#' @param birth_date Optional. The name of the column containing birth dates. Used to calculate age when `age_range` is specified. Requires `date_var` to be supplied. Age will be calculated as (date_var - birth_date)/365.25.
#' @param age Optional. The name of the column containing age values. Used directly for age filtering when `age_range` is specified.
#' @param age_range Optional. A length 2 numeric vector `c(min, max)` specifying the age range in years. Use `NA` for one-sided bounds (e.g., `c(10, NA)` for age >= 10, or `c(NA, 65)` for age <= 65). At least one non-NA value must be provided.
#' @param force_collect A logical for whether force downloading the result table if it is not a local data.frame. Downloading data could be slow, so the user has to opt in; default is FALSE.
#' @param verbose A logical for whether printing explanation for the operation. Default is fetching from options. Use `options(healthdb.verbose = FALSE)` to suppress once and for all.
#' @param ... Additional arguments, e.g., `mode`, passing to [restrict_date()].
#'
#' @return A subset of input data satisfied the specified case definition.
#' @export
#'
#' @examples
#' sample_size <- 30
#' df <- data.frame(
#'   clnt_id = rep(1:3, each = 10),
#'   service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1),
#'     size = sample_size, replace = TRUE
#'   ),
#'   diagx = sample(letters, size = sample_size, replace = TRUE),
#'   diagx_1 = sample(c(NA, letters), size = sample_size, replace = TRUE),
#'   diagx_2 = sample(c(NA, letters), size = sample_size, replace = TRUE)
#' )
#'
#' # define from one source
#' define_case_with_age(df,
#'   vars = starts_with("diagx"), "in", vals = letters[1:4],
#'   clnt_id = clnt_id, date_var = service_dt,
#'   excl_args = list(if_all = TRUE),
#'   # remove non-case
#'   mode = "filter",
#'   # keeping the first record
#'   keep = "first"
#' )
#'
#' # with age restriction using birth_date
#' df_with_birth <- df
#' df_with_birth$birth_dt <- as.Date("1990-01-01")
#' define_case_with_age(df_with_birth,
#'   vars = starts_with("diagx"), "in", vals = letters[1:4],
#'   clnt_id = clnt_id, date_var = service_dt,
#'   birth_date = birth_dt, age_range = c(18, 65),
#'   mode = "filter"
#' )
#'
#' # age restriction with one-sided bound (age >= 18 only)
#' define_case_with_age(df_with_birth,
#'   vars = starts_with("diagx"), "in", vals = letters[1:4],
#'   clnt_id = clnt_id, date_var = service_dt,
#'   birth_date = birth_dt, age_range = c(18, NA),
#'   mode = "filter"
#' )
#'
#' # multiple sources with purrr::pmap
#' # arguments with length = 1 will be recycle to match the number of sources
#' # wrap expressions/unquoted variables with bquote(),
#' # or rlang:exprs() to prevent immediate evaluation,
#' # or just use quoted variable names
#' purrr::pmap(
#'   list(
#'     data = list(df, df),
#'     vars = rlang::exprs(starts_with("diagx")),
#'     match = c("in", "start"),
#'     vals = list(letters[1:4], letters[5:10]),
#'     clnt_id = list(bquote(clnt_id)), n_per_clnt = c(2, 3),
#'     date_var = "service_dt",
#'     excl_vals = list(letters[11:13], letters[14:16]),
#'     excl_args = list(list(if_all = TRUE), list(if_all = FALSE))
#'   ),
#'   define_case_with_age
#' )
define_case_with_age <- function(data, vars, match = "in", vals, clnt_id, n_per_clnt = 1, date_var = NULL, apart = NULL, within = NULL, uid = NULL, excl_vals = NULL, excl_args = NULL, keep = c("all", "first", "last"), if_all = FALSE, mode = c("flag", "filter"), birth_date = NULL, age = NULL, age_range = NULL, force_collect = FALSE, verbose = getOption("healthdb.verbose"), ...) {
  stopifnot(rlang::is_named2(excl_args))

  rlang::check_required(clnt_id)

  # place holder for temp var names
  .age_calc <- NULL

  # capture variable names
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))

  mode <- rlang::arg_match0(mode, c("flag", "filter"))

  dot <- rlang::list2(...)

  has_date_var <- !rlang::quo_is_null(rlang::enquo(date_var))
  if (has_date_var) date_var <- rlang::as_name(rlang::enquo(date_var))
  if (!has_date_var & any(!is.null(apart), !is.null(within))) stop("'date_var' must be supplied if 'within'/'apart' is not NULL")

  has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
  if (has_uid) uid <- rlang::as_name(rlang::enquo(uid))

  has_birth_date <- !rlang::quo_is_null(rlang::enquo(birth_date))
  if (has_birth_date) birth_date <- rlang::as_name(rlang::enquo(birth_date))

  has_age <- !rlang::quo_is_null(rlang::enquo(age))
  if (has_age) age <- rlang::as_name(rlang::enquo(age))

  if (!is.null(age_range)) {
    if (length(age_range) != 2) stop("`age_range` must be a length 2 vector")
    if (all(is.na(age_range))) stop("`age_range` must have at least one non-NA value")
    if (!has_birth_date & !has_age) stop("Either `birth_date` or `age` must be supplied if `age_range` is not NULL")
    if (has_birth_date & has_age) stop("Only one of `birth_date` or `age` should be supplied, not both")
    if (has_birth_date & !has_date_var) stop("`date_var` must be supplied to calculate age from `birth_date`")
  }

  keep <- rlang::arg_match0(keep, c("all", "first", "last"))
  if (mode == "flag" & keep != "all") stop("'flag' mode does not allow subsetting with 'keep'.")
  if (keep != "all" & !has_date_var) stop("`date_var` must be supplied for sorting if not keeping all records")

  # capture the arguments to be re-used in two identify_row calls
  arg <- rlang::enexprs(vars, match, vals, if_all, verbose)
  names(arg) <- purrr::map_chr(rlang::exprs(vars, match, vals, if_all, verbose), rlang::as_label)
  # data is included separately since the call should be evaluate on the data already assigned to `data`,
  # instead of the original symbol of data in the global environment
  incl <- rlang::call2("identify_row", data = rlang::expr(data), !!!arg)
  incl <- rlang::call_modify(incl, ... = rlang::zap(), .homonyms = "first")

  # body
  if (verbose) rlang::inform(c(">" = "--------------Inclusion step--------------\n"))
  # browser()
  result <- eval(incl)

  if (!is.null(excl_vals)) {
    if (verbose) rlang::inform(c(">" = "--------------Exclusion step--------------\n"))
    # allow overwriting arguments for identifying exclusion values
    excl <- rlang::call_modify(incl, !!!excl_args, vals = excl_vals, .homonyms = "last")
    if (is.data.frame(result)) {
      if_report <- clnt_id
    } else {
      if_report <- NULL
    }
    result <- rlang::inject(result %>% exclude(!!excl, by = !!clnt_id, report_on = !!if_report, verbose = verbose))
  }

  if (n_per_clnt > 1) {
    if (verbose) rlang::inform(c(">" = "--------------No. rows restriction--------------\n"))
    result <- rlang::inject(result %>% restrict_n(clnt_id = !!clnt_id, n_per_clnt = n_per_clnt, count_by = !!ifelse(has_date_var, date_var, rlang::missing_arg()), mode = mode, verbose = verbose))
  }

  if (!is.null(age_range)) {
    if (verbose) rlang::inform(c(">" = "--------------Age restriction--------------\n"))

    if (has_age) {
      if (!is.na(age_range[1]) & !is.na(age_range[2])) {
        result <- result %>%
          dplyr::filter(.data[[!!age]] >= !!age_range[1] & .data[[!!age]] <= !!age_range[2])
      } else if (!is.na(age_range[1])) {
        result <- result %>%
          dplyr::filter(.data[[!!age]] >= !!age_range[1])
      } else if (!is.na(age_range[2])) {
        result <- result %>%
          dplyr::filter(.data[[!!age]] <= !!age_range[2])
      }
    } else if (has_birth_date) {
      # Calculate age from birth_date and date_var using difftime for SQL compatibility
      result <- result %>%
        dplyr::mutate(.age_calc = abs(difftime(.data[[!!date_var]], .data[[!!birth_date]], units = "days")) / 365.25)

      if (!is.na(age_range[1]) & !is.na(age_range[2])) {
        result <- result %>%
          dplyr::filter(.data[[".age_calc"]] >= !!age_range[1] & .data[[".age_calc"]] <= !!age_range[2])
      } else if (!is.na(age_range[1])) {
        result <- result %>%
          dplyr::filter(.data[[".age_calc"]] >= !!age_range[1])
      } else if (!is.na(age_range[2])) {
        result <- result %>%
          dplyr::filter(.data[[".age_calc"]] <= !!age_range[2])
      }

      result <- result %>%
        dplyr::select(-.age_calc)
    }
  }

  if (has_date_var & any(!is.null(apart), !is.null(within))) {
    if (verbose) rlang::inform(c(">" = "--------------Time span restriction--------------\n"))
    result <- rlang::inject(result %>% restrict_date(clnt_id = !!clnt_id, date_var = !!date_var, n = n_per_clnt, apart = apart, within = within, uid = !!uid, mode = mode, force_collect = force_collect, verbose = verbose, !!!dot))
  }

  if (verbose) rlang::inform(c(">" = paste("--------------", "Output", keep, "records--------------\n")))
  # switch to filter(date = min/max(date)) instead of problematic slice_min/max
  # fixing sql translation failed when using .data with slice_min/max
  # if (is.data.frame(result)) {
  #   date_var <- rlang::expr(.data[[!!date_var]])
  # } else {
  #   date_var <- rlang::expr(dbplyr::sql(dbplyr::escape(dbplyr::ident(!!date_var), con = dbplyr::remote_con(result))))
  #   # changed from above because translating slice_max failed
  #   # reversed to above again as of dbplyr 2.4.0
  #   # date_var <- rlang::expr(!!date_var)
  # }

  # replacing slice_ function in expression
  if (keep != "all") {
    expr_slice <- rlang::expr(
      result <- result %>%
        dplyr::group_by(.data[[!!clnt_id]]) %>%
        # dplyr::slice_min(!!date_var, n = 1, with_ties = FALSE) %>%
        # switch to filter(date = min/max(date)) instead of problematic slice_min/max
        dplyr::filter(.data[[!!date_var]] == min(.data[[!!date_var]], na.rm = TRUE))
      # row_number translation didn't work for MS SQL
      # dplyr::filter(dplyr::row_number() == 1) %>%
    ) %>%
      rlang::expr_text()

    if (keep == "last") expr_slice <- expr_slice %>% stringr::str_replace("min", "max")
    expr_slice <- expr_slice %>% rlang::parse_expr()
    eval(expr_slice)

    if (!is.data.frame(result)) {
      if (!has_uid) stop("`uid` must be supplied for sorting database if not keeping all records")
      result <- result %>%
        dplyr::filter(.data[[!!uid]] == min(.data[[!!uid]], na.rm = TRUE))
    } else {
      result <- result %>%
        dplyr::filter(dplyr::row_number() == 1)
    }

    result <- dplyr::ungroup(result)
  }

  if (force_collect & !is.data.frame(result)) result <- dplyr::collect(result)

  return(result)
}
