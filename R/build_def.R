#' Build case definition function calls
#'
#' @description
#' This function assembles function calls from the supplied functions, their required arguments, leaving the data argument empty for easy re-use of the definition calls with different data and batch execution (see ?execute_def for detail). It is useful for defining multiple diseases/events across multiple sources.
#'
#'
#' @param def_lab A single character label for the definition, e.g., some disease.
#' @param src_labs A character vector of place-holder names for the data sources that will be used to execute the definition.
#' @param def_fn A list of functions (default: list(define_case)) that will filter the source data sets and keep clients met the case definition. The length of the list should be either 1 or equal to the length of `src_labs`. If length = 1, the same function will be applied to all sources; otherwise, `def_fn` should match `src_lab` by position. User can supply custom functions but must name the first argument `data`.
#' @param fn_args A named list of arguments passing to the `def_fn`. Each element in the list should have the same name as an argument in the source-specific `def_fn`, and the element length should also be either 1 or equal to the number of sources. If you have `def_fn` functions taking different sets of arguments, include the union in one list. Only those that are valid to the source-specific function will be used in the resulted function call.
#'
#' @return A tibble with `length(src_labs)` rows, containing the input arguments and the synthetic function call in the `fn_call` column.
#' @export
#'
#' @examples
#' build_def("SUD", # usually a disease name
#'   src_lab = c("src1", "src2"), # identify from multiple sources, e.g., hospitalization, ED visits.
#'   # functions that filter the data with some criteria,
#'   # including mean here for src2 as a trivial example
#'   # to show only valid arguments will be in the call
#'   def_fn = list(define_case, mean),
#'   fn_args = list(
#'     # use bquote to pass tidyselect expressions
#'     vars = list(bquote(starts_with("diagx")), "diagx_2"),
#'     match = "start", # "start" will be applied to all sources as length = 1
#'     vals = list(c("304"), c("305")),
#'     clnt_id = "clnt_id",
#'     # c() can be used in place of list
#'     # if this argument only takes one value for each source
#'     n_per_clnt = c(2, 3),
#'     x = list(1:10) # src2 with mean as def_fn will only accept this argument
#'   )
#' )
build_def <- function(def_lab, src_labs, def_fn = list(define_case), fn_args) {
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

  # build calls with fn and args leaving data empty for re-usability (e.g. supplying different with the same def via execute_def)
  # unlist(recursive = FALSE) is necessary to prevent changing types of the variables in some cases
  # i.e., n_per_clnt = c(2, 3) may end up as c(2, "3") when unlisting
  df <- dplyr::bind_cols(df_data, df_fn, df_args) %>%
    tidyr::nest(fn_args = dplyr::any_of(names(df_args))) %>%
    dplyr::mutate(
      # use fn_fmls_names to filter valid arguments of the specific function
      fn_args = purrr::map2(.data[["def_fn"]], .data[["fn_args"]], function(x, y) unlist(y, recursive = FALSE)[names(y) %in% rlang::fn_fmls_names(rlang::parse_expr(x) %>% eval())]),
      fn_call = purrr::map2(.data[["def_fn"]], .data[["fn_args"]], function(x, y) rlang::call2(x, data = rlang::missing_arg(), rlang::splice(y)))
    )

  return(df)
}
