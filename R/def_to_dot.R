# guess the ... argument for bind_source from def
def_to_dot <- function(def, add_aux = TRUE) {
  stopifnot(identical(names(def), c("def_lab", "src_labs", "def_fn", "fn_args", "fn_call")))

  fn_args <- def_lab <- def_fn <- fn_call <- fn_arg_names <- n_per_clnt <- n <- src_labs <- NULL

  # n_source <- dplyr::n_distinct(def[["src_labs"]])

  if (any(!(def$def_fn %in% c("define_case", "healthdb::define_case", "define_case_with_age", "healthdb::define_case_with_age")))) warning("'def' was not built by define_case() or define_case_with_age(). Unexpected issues may occur. Please check output thoroughly.")

  key_args <- c("uid", "clnt_id", "date_var")
  flag_args <- c("n_per_clnt", "apart", "within")

  # turn def to wide form row = sources, col = variables to keep
  def_long <- def %>%
    tidyr::unnest(fn_args) %>%
    # dplyr::mutate(fn_arg_names = sapply(def$fn_args, function(x) names(x)) %>% c()) %>%
    dplyr::mutate(
      fn_arg_names = names(fn_args),
      src_labs = glue::glue("{def_lab}_{src_labs}")
    ) %>%
    dplyr::select(-def_lab, -def_fn, -fn_call) %>%
    dplyr::distinct() %>%
    dplyr::filter(fn_arg_names %in% c(key_args, flag_args))

  # not needed since result from execute_def has a copy of source for each def_src combination
  # so that variable names in same source can be different as long as the def was successfully executed
  # # deal with duplicate for multiple def
  # def_long_key <- def_long %>%
  #   dplyr::filter(fn_arg_names %in% key_args) %>%
  #   dplyr::group_by(src_labs, fn_arg_names) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L)
  # if (nrow(def_long_key) > 0) stop("Variable names for binding cannot be determined from definition due to multiple ID names (e.g., src1 has two different clnt_id under different def_lab) were assigned to the same source. If that was intended, give different src_lab for the same source, e.g., src1a = df1, src1b = df1, or do manual binding with bind_sources().")

  def_wide <- def_long %>% tidyr::pivot_wider(names_from = fn_arg_names, values_from = fn_args, values_fn = list)

  def_wide_keys <- def_wide %>%
    dplyr::select(dplyr::any_of(key_args)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) purrr::map_chr(unlist(x, recursive = FALSE), ~ ifelse(!is.null(.), as.character(.), NA_character_)))) %>%
    as.list()

  def_wide_flags <- def_wide %>% dplyr::select(dplyr::any_of(flag_args))

  # turn expression in cells to character
  if ("n_per_clnt" %in% names(def_wide_flags)) {
    def_wide_flags <- def_wide_flags %>%
      dplyr::mutate(flag_restrict_n = purrr::map_chr(n_per_clnt, ~ dplyr::if_else(max(unlist(.), na.rm = TRUE) > 1, "flag_restrict_n", NA_character_)))
  }
  if (any(c("apart", "within") %in% names(def_wide_flags))) {
    def_wide_flags <- def_wide_flags %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c("apart", "within")), function(x) purrr::map_chr(x, ~ dplyr::if_else(!is.null(unlist(.)), "flag_restrict_date", NA_character_))),
        flag_restrict_date = data.table::fcoalesce(dplyr::pick(dplyr::any_of(c("apart", "within"))))
      )
  }

  # overwrite flag by mode is not needed as filter mode also gives flag now
  # if("mode" %in% names(def_wide_flags)) {
  #   def_wide_flags <- def_wide_flags %>%
  #     dplyr::mutate(dplyr::across(dplyr::starts_with("flag_"),
  #                                 function(x) purrr::map2_chr(x, mode,
  #                                                             ~ dplyr::case_when(.y == "filter" ~ NA_character_,
  #                                                                         .default = .x))))
  # }

  if (nrow(def_wide_flags) > 0) {
    arg_list <- append(def_wide_keys, as.list(def_wide_flags %>% dplyr::select(dplyr::starts_with("flag_"))))
  } else {
    arg_list <- def_wide_keys
  }

  if (add_aux) arg_list <- append(list(def = "def", src = "src"), arg_list)

  arg_list <- purrr::map(arg_list, as.character)
  arg_list <- Filter(function(x) any(!is.na(x)), arg_list)

  return(arg_list)
}
