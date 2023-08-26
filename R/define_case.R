#' Identify diseases/events from administrative records
#'
#' @description
#' This function is a composite of identify_rows, exclude, and restrict_. It is aimed to implement case definition, e.g., two or more physician visits with some diagnostic code at least 30 days apart within two years, in one shot. The component functions are chained in the following order if all arguments were supplied (see the verbose output for what was done if some arguments are missing): identify_rows(vals) %>% exclude(identify_rows(excl_vals), by = clnt_id) %>% restrict_n() %>% restrict_dates()
#'
#' @param data Data frames or remote tables (e.g., from dbplyr)
#' @param vars An expression passing to `dplyr::select()`. It can be Quoted/unquoted column names, or `tidyselect` helper functions, such as `starts_with()`.
#' @param match One of "in", "start", "regex", "like", "between", and "glue_sql". It determines how values would be matched. See `identify_rows()` for detail.
#' @param vals Depending on `match`, it takes different input. See `identify_rows()`.
#' @param clnt_id Grouping variable (quoted/unquoted).
#' @param n_per_clnt A single number specifying the minimum number of group size. See `restrict_n()` for detail. It would also be used as the n argument for `restrict_dates()`.
#' @param date_var Variable name (quoted/unquoted) for the dates to be interpreted. If present, it would be used as the count_by argument for `restrict_n()`.
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw. See `restrict_dates()`.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param excl_vals Same as `vals` but groups with these values are going to be removed from the result.
#' @param excl_args A named list of arguments for the second `identify_rows()` call for `excl_vals`. If not supplied, `var`, `match` and `if_all` of the first call will be re-used.
#' @param keep One of "first" (keeping each client's earliest record), "last" (keeping the latest), and "all" (keeping all relevant records, default).
#' @param if_all A logical for whether combining the predicates (if multiple columns were selected by vars) with AND instead of OR. Default is FALSE, e.g., var1 in vals OR var2 in vals.
#' @param force_collect A logical for whether force downloading remote table if `apart` is not NULL. For remote table only, because `apart` is implemented for local data frame only. Downloading data could be slow, so the user has to opt in; default FALSE will stop with error.
#' @param verbose A logical for whether printing explanation for the operation. Default is fetching from options. Use options(odcfun.verbose = FALSE) to suppress once and for all.
#' @param ... Additional arguments passing to `restrict_dates()`.
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
define_case <- function(data, vars, match = "in", vals, clnt_id, n_per_clnt = 1, date_var = NULL, apart = NULL, within = NULL, excl_vals = NULL, excl_args = NULL, keep = c("all", "first", "last"), if_all = FALSE, force_collect = FALSE, verbose = getOption("odcfun.verbose"), ...) {
  stopifnot(rlang::is_named2(excl_args))

  rlang::check_required(clnt_id)

  # capture variable names
  clnt_id <- rlang::as_name(rlang::enquo(clnt_id))
  has_date_var <- !rlang::quo_is_null(rlang::enquo(date_var))
  if (has_date_var) date_var <- rlang::as_name(rlang::enquo(date_var))
  if (!has_date_var & any(!is.null(apart), !is.null(within))) stop("'date_var' must be supplied if 'within'/'apart' is not NULL")

  keep <- rlang::arg_match0(keep, c("all", "first", "last"))
  if (keep != "all" & !has_date_var) stop("`date_var` must be supplied for sorting if not keeping all records")

  # capture the arguments to be re-used in two identify_rows calls
  arg <- rlang::enexprs(data, vars, match, vals, if_all, verbose)
  names(arg) <- purrr::map_chr(rlang::exprs(data, vars, match, vals, if_all, verbose), rlang::as_label)
  incl <- rlang::call2("identify_rows", !!!arg)
  incl <- rlang::call_modify(incl, ... = rlang::zap(), .homonyms = "first")

  # body
  if (verbose) cat("\n--------------Inclusion step--------------\n")
  result <- eval(incl)

  if (!is.null(excl_vals)) {
    if (verbose) cat("\n--------------Exclusion step--------------\n")
    # allow overwriting arguments for identifying exclusion values
    excl <- rlang::call_modify(incl, !!!excl_args, vals = excl_vals, .homonyms = "last")
    result <- rlang::inject(result %>% exclude(!!excl, by = !!clnt_id, report_on = !!clnt_id, verbose = verbose))
  }

  if (n_per_clnt > 1) {
    if (verbose) cat("\n--------------No. rows restriction--------------\n")
    result <- rlang::inject(result %>% restrict_n(clnt_id = !!clnt_id, n_per_clnt = n_per_clnt, count_by = !!ifelse(has_date_var, date_var, rlang::missing_arg()), verbose = verbose))
  }

  if (has_date_var & any(!is.null(apart), !is.null(within))) {
    if (verbose) cat("\n--------------Time span restriction--------------\n")
    result <- rlang::inject(result %>% restrict_dates(clnt_id = !!clnt_id, date_var = !!date_var, n = n_per_clnt, apart = apart, within = within, force_collect = force_collect, verbose = verbose, ...))
  }

  if (verbose) cat("\n--------------", "Output", keep, "records--------------\n")
  # fixing sql translation failed when using .data with slice_min/max
  if (is.data.frame(result)) {
    date_var <- rlang::expr(.data[[!!date_var]])
  } else {
    date_var <- rlang::expr(dbplyr::sql(dbplyr::escape(dbplyr::ident(!!date_var), con = dbplyr::remote_con(result))))
  }
  #browser()

  # replacing slice_ function in expression
  if (keep != "all") {
    expr_slice <- rlang::expr(result <- result %>%
                                dplyr::group_by(.data[[!!clnt_id]]) %>%
                                dplyr::slice_min(!!date_var, n = 1, with_ties = FALSE) %>%
                                dplyr::ungroup()) %>%
      rlang::expr_text()
    if (keep == "last") expr_slice <- expr_slice %>% stringr::str_replace("slice_min", "slice_max")
    expr_slice <- expr_slice %>% rlang::parse_expr()
    eval(expr_slice)
  }

  if (force_collect) result <- dplyr::collect(result, cte = TRUE)

  return(result)
}


# pmap(list(data = list(db, test_dat), vars = exprs(starts_with("diagx")), match = c("in", "start"), vals = list(c("304"), c("305")),  clnt_id = exprs(clnt_id, clnt_id), n_per_clnt = c(2, 3), excl_vals = list(c("50B"), c("304")), excl_args = list(list(if_all = TRUE), list(if_all = FALSE))), define_case)

# define_case_per_source <- function(..., source, clnt_id_nm, from_var_nm, incl_vals, n_per_clnt, excl_vals, date_nm, dates_apart, dates_within, multi_var_cols, verbose) {
#
#   rlang::enexprs(...)
#
# }
#
# define_case <- function(..., .fn_identify_incl, .fn_identify_excl, .fn_excl, .fn_restrict)
#
# define_across <- function(..., clnt_id_nm, from_var_nm, incl_vals, excl_vals, n_per_clnt, date_nm, dates_apart, dates_within, multi_var_cols, verbose) {
#   sources <- list(...)
#   stopifnot(sapply(sources, function(x) class(x) %in% c("tbl_sql", "data.frame")))
#   n_sources <- length(sources)
#   stopifnot(lengths(list(from_var_nm, incl_vals, excl_vals, n_per_clnt, date_nm, dates_apart, dates_within, multi_var_cols)) %in% c(1, n_sources))
#   purrr::walk(list(from_var_nm, incl_vals, excl_vals, n_per_clnt, date_nm, dates_apart, dates_within, multi_var_cols), process_largs)
#
# }
#
# process_largs <- function(arg, len, verbose) {
#   if (is.list(arg)) {
#     if (length(arg) != len) stop("Argument ", deparse(substitute(arg)), " is not of the same lenght as the sources. Unlist it if you want to repeat a single value/vector for all the sources.")
#   }
#   else {
#     if (verbose) cat("\nArgument", deparse(substitute(arg)), "is not a list and treated as the same value for each source.\n")
#     assign(deparse(substitute(arg)), rep(list(arg), len), envir = parent.frame())
#   }
# }
#
# test_f <- function(what, len, verbose) {
#   process_args(what, len, verbose)
#   print(what)
# }
