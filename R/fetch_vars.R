#' Get variables from multiple tables with common ID columns
#'
#' @md
#' @description
#' This function calls [dplyr::left_join()] multiple times with different source tables (y argument of the join) to gather variables. It is not intended to replace left_join but simplify syntax for the situation that you need to get variables from multiple tables, and the tables can be linked by common IDs, which is often the case when working with relational databases. That said, this function is only meant for simple joins and only allows one-to-one matching.
#'
#'
#' @param data A local data.frame, or tibble. It would be used as the x argument in left_join.
#' @param keys A vector of quoted/unquoted variable names, or tidyselect expression (see [dplyr::select()]). These variables must be present in `data` and would be used as the `by` argument in left_join. The y tables must have a subset of these if not all.
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
#' @param verbose A logical for whether report the number of rows after joining for each source. Default is getting from options. Use options(odcfun.verbose = FALSE) to suppress once and for all.
#' @param ... Additional arguments passing to left_join.
#'
#' @return A data.frame or tibble containing all original columns of x and new variables matched from other tables based on the specified linkage.
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
#' fetch_vars(df1,
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
#' fetch_vars(df1,
#'   keys = c(id, year),
#'   linkage = list(
#'     db2 ~ starts_with("s"),
#'     df3 ~ sex | id
#'   ),
#'   copy = TRUE # pass to left_join for forced collection of remote table
#' )
fetch_vars <- function(data, keys, linkage, verbose = getOption("odcfun.verbose"), ...) {
  # input checks
  stopifnot(any(purrr::map_lgl(linkage, rlang::is_formula)))

  # place holder for helper variable names
  vars <- y <- keys_y <- NULL

  # capture expression arguments
  data_quo <- rlang::enquo(data)
  data_expr <- rlang::quo_get_expr(data_quo)
  data_env <- rlang::quo_get_env(data_quo)

  stopifnot(is.data.frame(data))

  keys <- dplyr::select(data, {{ keys }}) %>% names()

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

  x_arg <- glue::glue("dplyr::select({data_expr}, c({glue::glue_collapse(keys, ', ')}))") %>% rlang::parse_expr()

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(calls = rlang::call2("left_join", x = x_arg, y = y, by = keys_y, .ns = "dplyr") %>% list())

  if (!rlang::is_empty(dots)) {
    mod_calls <- purrr::map(df[["calls"]], function(x) rlang::call_modify(x, !!!dots, .homonyms = "last"))
  } else {
    mod_calls <- df[["calls"]]
  }

  vars_df <- purrr::map(mod_calls, function(x) eval(x, envir = data_env) %>% dplyr::select(-dplyr::any_of(keys)))

  y_n <- purrr::map_dbl(vars_df, nrow)

  if (verbose) cat("\nThe data has", nrow(data), "rows. After joining,", glue::glue("variable(s) from {df$lhs} has {y_n} rows") %>% paste(collapse = ", and "), "\n")

  vars_df <- purrr::list_cbind(vars_df)

  result <- dplyr::bind_cols(data, vars_df)

  return(result)
}
