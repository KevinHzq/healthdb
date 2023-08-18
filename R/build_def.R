build_def <- function(def_lab, src_labs, def_fn, fn_args) {
  # capture names of the functions before any eval even before checking
  fn_exprs <- rlang::try_fetch(rlang::enquo(def_fn) %>% rlang::call_args(),
    error = function(cnd) {
      rlang::abort("The 'def_fn' functions need to be unquoted and wrapped in list()", parent = cnd)
    }
  )
  fn_labs <- purrr::map_chr(fn_exprs, rlang::as_name)

  # input checking
  stopifnot(
    rlang::is_character(def_lab),
    rlang::is_character(src_labs),
    rlang::is_list(fn_args),
    purrr::map_lgl(def_fn, rlang::is_function),
    rlang::is_named2(fn_args),
    any(lengths(fn_args) %in% c(1, length(src_labs)))
  )

  # build separate tibbles with the arguments
  # separate because later bind_cols can auto recycle shorter frames to match the longer ones
  df_data <- tidyr::expand_grid(def_lab, src_labs)
  df_fn <- dplyr::tibble(`def_fn` = fn_labs)
  df_args <- dplyr::as_tibble(fn_args)
  # print(df_args)

  # get the arguments that are not supplied (not needed anymore)
  # extra_args <- Filter(Negate(rlang::is_missing), rlang::fn_fmls(def_fn))
  # #extra_args <- rlang::call_args(rlang::call2(def_fn, !!!rlang::fn_fmls(def_fn)))
  # df_extra <- dplyr::as_tibble(map(extra_args, list)) %>%
  #   select(-any_of(colnames(df_args)))
  #
  # print(df_extra)

  #build calls with fn and args leaving data empty for re-usability (e.g. supplying different with the same def via execute_def)
  #unlist(recursive = FALSE) is necessary to prevent changing types of the variables in some cases
  #i.e., n_per_clnt = c(2, 3) may end up as c(2, "3") when unlist
  df <- dplyr::bind_cols(df_data, df_fn, df_args) %>%
    tidyr::nest(fn_args = dplyr::any_of(names(df_args))) %>%
    dplyr::mutate(fn_call = purrr::map2(.data[["def_fn"]], .data[["fn_args"]], function(x, y) rlang::call2(x, data = rlang::missing_arg(), rlang::splice(unlist(y, recursive = FALSE)))))

  return(df)
}
