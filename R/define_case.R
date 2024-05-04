#' Identify diseases/events from administrative records
#'
#' @md
#' @description
#' This function is a composite of [identify_row()], [exclude()], [restrict_n()], and [restrict_date()]. It is aimed to implement common case definitions in epidemiological studies using administrative database as a one-shot big query. The intended use case is for definitions in the form of, e.g., two or more physician visits with some diagnostic code at least 30 days apart within two years. The component functions mentioned above are chained in the following order if all arguments were supplied: `identify_row(vals) %>% exclude(identify_row(excl_vals), by = clnt_id) %>% restrict_n() %>% restrict_date()`. Only necessary steps in the chain will be ran if some arguments are missing, see the verbose output for what was done. Note that if `date_var` is supplied, `n_per_clnt` will be counted by distinct dates instead of number of records.
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
#' define_case(df,
#'   vars = starts_with("diagx"), "in", vals = letters[1:4],
#'   clnt_id = clnt_id, date_var = service_dt,
#'   excl_args = list(if_all = TRUE),
#'   # remove non-case
#'   mode = "filter",
#'   # keeping the first record
#'   keep = "first"
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
#'   define_case
#' )
define_case <- function(data, vars, match = "in", vals, clnt_id, n_per_clnt = 1, date_var = NULL, apart = NULL, within = NULL, uid = NULL, excl_vals = NULL, excl_args = NULL, keep = c("all", "first", "last"), if_all = FALSE, mode = c("flag", "filter"), force_collect = FALSE, verbose = getOption("healthdb.verbose"), ...) {
  stopifnot(rlang::is_named2(excl_args))

  rlang::check_required(clnt_id)

  # capture variable names
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))

  mode <- rlang::arg_match0(mode, c("flag", "filter"))

  dot <- rlang::list2(...)

  has_date_var <- !rlang::quo_is_null(rlang::enquo(date_var))
  if (has_date_var) date_var <- rlang::as_name(rlang::enquo(date_var))
  if (!has_date_var & any(!is.null(apart), !is.null(within))) stop("'date_var' must be supplied if 'within'/'apart' is not NULL")

  has_uid <- !rlang::quo_is_null(rlang::enquo(uid))
  if (has_uid) uid <- rlang::as_name(rlang::enquo(uid))

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
