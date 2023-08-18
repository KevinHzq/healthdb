execute_def <- function(def, with_data, bind = FALSE, force_proceed = FALSE) {
  with_data_expr <- rlang::enquo(with_data) %>% rlang::call_args()
  with_data_env <- rlang::enquo(with_data) %>% rlang::quo_get_env()

  # input checks
  stopifnot(
    rlang::is_named(with_data),
    rlang::is_list(with_data),
    !rlang::is_empty(with_data),
    identical(names(def), c("def_lab", "src_labs", "def_fn", "fn_args", "fn_call")),
    all(names(with_data) %in% def[["src_labs"]])
  )

  n_source <- dplyr::n_distinct(def[["src_labs"]])
  n_data <- length(with_data_expr)
  if (n_data != n_source) stop("'def' has ", n_source, " sources, but ", n_data, " source datasets are supplied.")

  # ask user input to proceed as collecting remote table may be slow
  # don't ask if all table is remote/local
  is_local <- purrr::map_lgl(with_data, function(x) "data.frame" %in% class(x))
  any_local <- any(is_local)
  any_remote <- any(!is_local)

  if (!force_proceed & bind & any_local & any_remote) {
    proceed <- readline(prompt = "Remote tables have to be collected (may be slow) in order to be binded. Proceed? [y/n]")

    if (proceed == "n") stop("Try bind = FALSE, or supply data from the same source (i.e., either all local or all remote).")
  }

  # first alter the call to include data, then eval
  def <- def %>%
    dplyr::mutate(
      fn_call = purrr::map2(
        .data[["fn_call"]], .data[["src_labs"]],
        function(x, y) rlang::call_modify(x, data = with_data_expr[[y]])
      ),
      result = purrr::map(.data[["fn_call"]], function(x) eval(x, envir = with_data_env), .progress = TRUE),
      result = purrr::pmap(
        list(.data[["result"]], .data[["def_lab"]], .data[["src_labs"]]),
        function(dat, def, src) {
          dat %>% mutate(
            `def` = def,
            `src` = src,
            .before = 1
          )
        }
      )
    )

  # result <- purrr::map(def[["fn_call"]], function(x) eval(x, envir = with_data_env))

  if (bind) {
    if (any_remote & any_local) {
      def <- def %>%
        mutate(
          result = purrr::map(.data[["result"]], dplyr::as_tibble, .progress = TRUE)
        )
    }

    if (!any_local) result <- Reduce(dplyr::union, def[["result"]])
    else result <- data.table::rbindlist(def[["result"]], use.names = FALSE)

    return(result)
  }

  return(def[["result"]])
}
