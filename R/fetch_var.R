#' Get variables from multiple tables with common ID columns
#'
#' @md
#' @description
#' This function fetches variables from different tables that are linked by common IDs. It calls [dplyr::left_join()] multiple times with various source tables (y argument of the join) to gather variables. It is not meant to replace left_join() but simplify syntax for the situation that you started off with a table of your study sample and wanted to gather covariates from different sources linked by common client IDs, which is often the case when working with healthcare databases.
#' **Caution**: this function is intended for one-to-one joins only because it could be problematic when we do not know which source caused a one-to-many join and changed the number of rows. For data.frame input, an error will be given when one-to-many joins were detected. However, such checking could be an expensive operation on remote source. Therefore, for database input, the result will not be checked.
#'
#' The names of the fetched variables must not clash with each other, with columns already in `data`, or with any key not used in their linkage (see the `|` syntax below); an error will be given in these cases, as the clashing columns would otherwise be silently renamed (e.g., `var.x`/`var.y`) with results that no longer mean what their names suggest. Also note that `|` is reserved for separating variables and keys in the linkage formulas and thus cannot be used within the 'tidyselect' expressions, e.g., use `c(starts_with("a"), ends_with("b"))` instead of `starts_with("a") | ends_with("b")`.
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
  stopifnot(all(purrr::map_lgl(linkage, rlang::is_formula)))

  # place holder for helper variable names
  vars <- y <- keys_y <- . <- NULL

  # capture expression arguments
  data_quo <- rlang::enquo(data)
  data_env <- rlang::quo_get_env(data_quo)

  # stopifnot(is.data.frame(data))
  is_df <- is.data.frame(data)
  if (!is_df) {
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
    keys_y = purrr::map(keys_y, function(x) if (all(x == "")) keys else unique(x)),
    keys_y_expr = purrr::map_chr(keys_y, function(x) glue::glue_collapse(x, ", ")),
    keys_y_len = purrr::map_dbl(keys_y, length)
  )

  if (max(df[["keys_y_len"]]) > length(keys)) stop("The length of variables supplied after '|' cannot be larger than the length of keys")
  if (any(!(df[["keys_y"]] %>% unlist() %in% keys))) stop("The variables supplied after '|' should be a subset of keys")

  # resolve the variables each linkage would fetch, and stop on name collisions;
  # otherwise left_join/bind_cols would silently rename the clashing columns
  # (e.g., var.x/var.y or var...2), leaving output names that no longer mean what they appear to
  fetched_vars <- purrr::pmap(
    list(df[["lhs"]], df[["vars"]], df[["keys_y"]]),
    function(lhs, vars, keys_y) {
      y_tab <- eval(lhs, envir = data_env)
      dplyr::select(y_tab, !!vars) %>%
        colnames() %>%
        setdiff(keys_y)
    }
  )

  purrr::pwalk(
    list(df[["lhs"]], fetched_vars, df[["keys_y"]]),
    function(lhs, fetched, keys_y) {
      clash <- intersect(fetched, setdiff(keys, keys_y))
      if (length(clash) > 0) stop(glue::glue("Variable(s) ({glue::glue_collapse(clash, ', ')}) fetched from `{rlang::expr_text(lhs)}` would collide with keys not included in its linkage. Rename or deselect them."), call. = FALSE)
      clash <- intersect(fetched, setdiff(colnames(data), keys))
      if (length(clash) > 0) stop(glue::glue("Variable(s) ({glue::glue_collapse(clash, ', ')}) fetched from `{rlang::expr_text(lhs)}` already exist in `data`. Rename or deselect them."), call. = FALSE)
    }
  )

  all_fetched <- unlist(fetched_vars)
  dup_fetched <- unique(all_fetched[duplicated(all_fetched)])
  if (length(dup_fetched) > 0) stop(glue::glue("Variable(s) ({glue::glue_collapse(dup_fetched, ', ')}) would be fetched from more than one source. Rename or deselect them."), call. = FALSE)

  # make join calls
  df <- df %>%
    dplyr::mutate(y = glue::glue("dplyr::select({lhs}, c({vars}, {keys_y_expr}))") %>% rlang::parse_exprs())

  if (is_df) {
    # use the data value directly (not a re-evaluated expression) and tag rows,
    # so matched values are re-aligned by row id later instead of trusting
    # left_join to preserve the row order of x
    rid_nm <- ".fetch_var_row_id"
    while (rid_nm %in% c(colnames(data), all_fetched)) rid_nm <- paste0(rid_nm, ".")
    x_arg <- dplyr::select(data, dplyr::all_of(keys))
    x_arg[[rid_nm]] <- seq_len(nrow(x_arg))
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


  if (is_df) {
    vars_df <- purrr::map(mod_calls, function(x) eval(x, envir = data_env))

    # every row id must come back exactly once; catches joins that added rows
    one_to_n <- purrr::map_lgl(vars_df, function(d) !identical(sort(d[[rid_nm]]), seq_len(nrow(data))))

    if (any(one_to_n)) rlang::abort(glue::glue('The join between data and any of ({stringr::str_flatten_comma(as.character(df$lhs[one_to_n]), last = " and ")}) is not one to one.'))

    # restore data's row order by the id, then drop keys and the id
    vars_df <- purrr::map(vars_df, function(d) {
      d[order(d[[rid_nm]]), , drop = FALSE] %>% dplyr::select(-dplyr::any_of(c(keys, rid_nm)))
    })

    vars_df <- purrr::list_cbind(vars_df)

    result <- dplyr::bind_cols(data, vars_df)
  } else {
    expr_vec <- purrr::map_chr(mod_calls, rlang::expr_text)
    expr_vec <- stringr::str_remove_all(expr_vec, stringr::fixed("x = ., "))
    pipe_expr_vec <- paste(c(rlang::as_name(data_quo), expr_vec), collapse = " %>% ")
    result <- eval(rlang::parse_expr(pipe_expr_vec), envir = data_env)
  }

  return(result)
}
