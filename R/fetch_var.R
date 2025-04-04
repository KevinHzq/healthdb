#' Get variables from multiple tables with common ID columns
#'
#' @md
#' @description
#' This function fetches variables from different tables that linked by common IDs. It calls [dplyr::left_join()] multiple times with various source tables (y argument of the join) to gather variables. It is not meant to replace left_join() but simplify syntax for the situation that you started off a table of study sample and wanted to gather covariates from different sources linked by common client IDs, which is often the case when working with healthcare databases.
#' **Caution**: this function is intended for one-to-one joins only because it could be problematic when we do not know which source caused a one-to-many join and changed the number of rows. For data.frame input, an error will be given when one-to-many joins were detected. However, such checking could be an expensive operation on remote source. Therefore, for database input, the result will not be checked.
#'
#'
#' @param data A data.frame or remote table (tbl_sql) which must be an object and not from a pipe. It would be used as the x argument in left_join().
#' @param keys A vector of quoted/unquoted variable names, or 'tidyselect' expression (see [dplyr::select()]). These variables must be present in `data` and would be used as the `by` argument in left_join(). The y tables must have a subset of these if not all.
#' @param linkage A list of formulas in the form of "from_tab ~ get_vars|by_keys":
#'  - source table on the left-hand-side
#'  - variables on the right-hand-side
#'  - If a source table does not have all the variables in `keys`, use "|" on RHS to specify the subset of `keys` to be used.
#'
#'  For example, given `keys` has 3 variables,
#'   list(
#'    y1 ~ tidyselect_expr1,
#'    y2 ~ tidyselect_expr2|key1 + key2)
#'
#'  meaning:
#'  1. from table y1 get variables picked by the tidyselect expression matching on all 3 keys;
#'  2. from table y2 get variables matching on only key1 and key2.
#' @param ... Additional arguments, e.g., `copy = TRUE`, passing to left_join().
#'
#' @return A data.frame or remote table containing all original columns of x and new variables matched from other tables based on the specified linkage.
#' @export
#'
#' @examples
#' # make toy data
#' size <- 30
#' n <- 10
#' df1 <- data.frame(
#'   id = sample(1:n, size = size, replace = TRUE),
#'   service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2022-01-31"), by = 1),
#'     size = size
#'   )
#' ) %>%
#'   dplyr::mutate(year = lubridate::year(service_dt))
#' df2 <- data.frame(
#'   id = rep(1:n, size / n), year = rep(2020:2022, each = n),
#'   status_1 = sample(0:1, size = size, replace = TRUE),
#'   status_2 = sample(0:1, size = size, replace = TRUE)
#' )
#' df3 <- data.frame(id = 1:n, sex = sample(c("F", "M"), size = n, replace = TRUE))
#'
#' # simple joins
#' # note that for left_join(df1, df2), boths keys have to be used,
#' # otherwise, error as the relation would not be one-to-one
#' fetch_var(df1,
#'   keys = c(id, year),
#'   linkage = list(
#'     df2 ~ starts_with("s"), # match both keys without '|'
#'     df3 ~ sex | id
#'   ) # match by id only; otherwise failed because df3 has no year
#' )
#'
#' # example if some y is remote
#' # make df2 as database table
#' db2 <- dbplyr::tbl_memdb(df2)
#'
#' fetch_var(df1,
#'   keys = c(id, year),
#'   linkage = list(
#'     db2 ~ starts_with("s"),
#'     df3 ~ sex | id
#'   ),
#'   copy = TRUE # pass to left_join for forced collection of remote table
#' )
fetch_var <- function(data, keys, linkage, ...) {
  # input checks
  stopifnot(any(purrr::map_lgl(linkage, rlang::is_formula)))

  # place holder for helper variable names
  vars <- y <- keys_y <- . <- NULL

  # capture expression arguments
  data_quo <- rlang::enquo(data)
  data_expr <- rlang::quo_get_expr(data_quo)
  data_env <- rlang::quo_get_env(data_quo)

  # stopifnot(is.data.frame(data))
  is_df <- is.data.frame(data)
  if(!is_df) {
    check_con(data)
  }

  keys <- dplyr::select(data, {{ keys }}) %>% colnames()

  dots <- rlang::list2(...)

  # make df for parsing the formulas by sources (y)
  df <- dplyr::tibble(lhs = purrr::map(linkage, rlang::f_lhs), rhs = purrr::map_chr(linkage, rlang::f_text))

  rhs_split <- stringr::str_split_fixed(df[["rhs"]], "\\|", n = 2)

  df <- df %>% dplyr::mutate(
    vars = rhs_split[, 1],
    vars = rlang::parse_exprs(vars),
    keys_y = purrr::map(rhs_split[, 2], function(x) stringr::str_split_1(x, "\\+") %>% stringr::str_trim()),
    keys_y = purrr::map(keys_y, function(x) if (all(x == "")) keys else x),
    keys_y_expr = purrr::map_chr(keys_y, function(x) glue::glue_collapse(x, ", ")),
    keys_y_len = purrr::map_dbl(keys_y, length)
  )

  if (max(df[["keys_y_len"]]) > length(keys)) stop("The length of variables supplied after '|' cannot be larger than the length of keys")
  if (any(!(df[["keys_y"]] %>% unlist() %in% keys))) stop("The variables supplied after '|' should be a subset of keys")

  # make join calls
  df <- df %>%
    dplyr::mutate(y = glue::glue("dplyr::select({lhs}, c({vars}, {keys_y_expr}))") %>% rlang::parse_exprs())

  if (is_df) {
    x_arg <- glue::glue("dplyr::select({data_expr}, c({glue::glue_collapse(keys, ', ')}))") %>% rlang::parse_expr()
  } else {
    x_arg <- rlang::expr(.)
  }

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(calls = rlang::call2("left_join", x = x_arg, y = y, by = keys_y, .ns = "dplyr") %>% list())

  # passing ... to left_join by modding calls
  if (!rlang::is_empty(dots)) {
    mod_calls <- purrr::map(df[["calls"]], function(x) rlang::call_modify(x, !!!dots, .homonyms = "last"))
  } else {
    mod_calls <- df[["calls"]]
  }

  # browser()

  if (is_df) {
    vars_df <- purrr::map(mod_calls, function(x) eval(x, envir = data_env) %>% dplyr::select(-dplyr::any_of(keys)))

    y_n <- purrr::map_dbl(vars_df, nrow)

    one_to_n <- y_n != nrow(data)

    if (any(one_to_n)) rlang::abort(glue::glue('The join between data and any of ({stringr::str_flatten_comma(as.character(df$lhs[one_to_n]), last = " and ")}) is not one to one.'))

    vars_df <- purrr::list_cbind(vars_df)

    result <- dplyr::bind_cols(data, vars_df)
  } else {
    expr_vec <- purrr::map_chr(mod_calls, rlang::expr_text)
    expr_vec <- stringr::str_remove_all(expr_vec, "x = ., ")
    pipe_expr_vec <- paste(c(rlang::as_name(data_quo), expr_vec), collapse = " %>% ")
    result <- eval(rlang::parse_expr(pipe_expr_vec), envir = data_env)
  }

  return(result)
}
