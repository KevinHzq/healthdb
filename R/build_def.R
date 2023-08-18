build_def <- function(def_lab, src_labs, fn_labs, fn_args = NULL) {
  #dots <- rlang::dots_list(..., .preserve_empty = TRUE)
  #print(enquo(def_fn) %>% rlang::eval_tidy())

  stopifnot(
            rlang::is_character(def_lab),
            #!rlang::is_empty(dots),
            #rlang::is_function(def_fn),
            rlang::is_named2(fn_args),
            any(lengths(fn_args) %in% c(1, length(src_labs)))
            )

  #data <- rlang::enexprs(...)
  #data <- purrr::map_chr(data_exprs, rlang::as_label)
  #print(names(data))
  df_data <- tidyr::expand_grid(def_lab, src_labs)
  df_fn <- dplyr::tibble(`fn_labs` = fn_labs)
  df_args <- dplyr::as_tibble(fn_args)
  #print(df_args)

  #get the arguments that are not supplied
  # extra_args <- Filter(Negate(rlang::is_missing), rlang::fn_fmls(def_fn))
  # #extra_args <- rlang::call_args(rlang::call2(def_fn, !!!rlang::fn_fmls(def_fn)))
  # df_extra <- dplyr::as_tibble(map(extra_args, list)) %>%
  #   select(-any_of(colnames(df_args)))
  #
  # print(df_extra)

  df <- dplyr::bind_cols(df_data, df_fn, df_args) %>%
    #dplyr::group_by(def_lab) %>%
    tidyr::nest(fn_args = dplyr::any_of(names(df_args))) %>%
    dplyr::mutate(fn_call = purrr::map2(.data$fn_labs, .data$fn_args, function(x, y) rlang::call2(x, data = rlang::missing_arg(), rlang::splice(unlist(y)))))
    # dplyr::mutate(new_fn = purrr::map2(fn_args, def_fn, function(x, y) rlang::new_function(rlang::dots_list(data = rlang::missing_arg(), rlang::splice(unlist(x)), rlang::splice(rlang::fn_fmls(y)), .preserve_empty = TRUE, .homonyms = "first"), body(y))))
    #dplyr::mutate(src_fn = purrr::map2(fn_args, def_fn, function(x, y) rlang::new_function(rlang::dots_list(!!!rlang::fn_fmls(rlang::enquo(y)), rlang::splice(unlist(x)), .preserve_empty = TRUE, .homonyms = "last"), body(y))))
  return(df)
}

