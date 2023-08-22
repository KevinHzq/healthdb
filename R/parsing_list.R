#inspired by the 'non-approach' from (https://rlang.r-lib.org/reference/topic-multiple-columns.html#using-external-defusal)

#although this is not recommended for defusing multiple columns in a single function argument in tidyselect context. It is useful for getting expressions from a single list argument for building calls

parsing_list <- function(list_arg, expr_only = TRUE) {
  if(!rlang::is_quosure(list_arg)) list_arg_quo <- rlang::enquo(list_arg)
  else list_arg_quo <- list_arg

  if (rlang::quo_is_call(list_arg_quo, "list") | rlang::quo_is_call(list_arg_quo, "c")) {
    expr <- rlang::quo_get_expr(list_arg_quo)
    env <- rlang::quo_get_env(list_arg_quo)
    args <- rlang::as_quosures(rlang::call_args(expr), env = env)
  }
  else {
    args <- rlang::enquos(list_arg)
  }

  if (expr_only) {
    args <- purrr::map(args, rlang::quo_get_expr)
  }

  return(args)
}
