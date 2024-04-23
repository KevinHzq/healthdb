#' Row-bind a list of data.frames or remote tables
#'
#' @md
#' @param data A list of data.frame or remote tables, e.g., output from [execute_def()].
#' @param ... Named arguments for each variable included in the output. The argument name should be the new name in the output, and the right hand side of the argument is a character vector of the original names. The name vector and the list elements in `data` will be matched by position. if an output variable only came from some of the sources, fill the name vector to a length equal to the number of sources with NA, e.g., `var` only come from the second out of three sources, then `var = c(NA, 'nm_in_src2', NA)`.
#' @param force_proceed A logical for whether to ask for user input in order to proceed when remote tables are needed to be collected for binding. The default is TRUE to let user be aware of that the downloading process may be slow. Use `options(odcfun.force_proceed = FALSE)` to suppress the prompt once and for all.
#'
#' @return A data.frame or remote table containing combined rows of the input list with variables specified by ...
#' @export
#'
#' @examples
#' df1 <- subset(iris, Species == "setosa")
#' df2 <- subset(iris, Species == "versicolor")
#' df3 <- subset(iris, Species == "virginica")
#'
#' bind_sources(list(df1, df2, df3),
#'   s_l = "Sepal.Length",
#'   s_w = "Sepal.Width",
#'   p_l_setosa = c("Petal.Length", NA, NA),
#'   p_l_virginica = c(NA, NA, "Petal.Length")
#' )
bind_sources <- function(data, ..., force_proceed = getOption("odcfun.force_proceed")) {
  # capture data names in the original env before any eval
  data_quo <- rlang::enquo(data)
  is_list_obj <- rlang::quo_is_symbol(data_quo)
  if (is_list_obj) {
    data_expr <- rlang::quo_get_expr(data_quo) %>% rlang::as_name()
    n_data <- length(data)
    data_expr <- lapply(1:n_data, function(x) glue::glue("{data_expr}[[{x}]]") %>% rlang::parse_expr())
  } else {
    data_expr <- data_quo %>% rlang::call_args()
    n_data <- length(data_expr)
  }
  data_env <- data_quo %>% rlang::quo_get_env()


  # input checks
  stopifnot(
    rlang::is_list(data),
    !rlang::is_empty(data)
  )

  var_list <- rlang::list2(...)
  var_len <- lengths(var_list)

  if (any(!(var_len %in% c(1, n_data)))) stop("The number of variable names does not match the number of sources. If a variable only came from some of the sources, fill the name vector to a length equal to the number of sources with NA, e.g., 'var' only come from the first out of three sources, var = c('nm_in_src1', NA, NA).")

  var_tab <- data.table::as.data.table(var_list)
  var_arg <- lapply(1:nrow(var_tab), function(i) as.list(var_tab[i]))
  var_arg <- lapply(1:nrow(var_tab), function(i) var_arg[[i]][!is.na(var_arg[[i]])])
  select_calls <- lapply(1:length(data), function(j) rlang::call2("select", .data = data_expr[[j]], !!!var_arg[[j]], .ns = "dplyr"))

  result <- purrr::map(select_calls, function(x) eval(x, envir = data_env))

  # ask user input to proceed as collecting remote table may be slow
  # don't ask if all table is remote/local
  is_local <- purrr::map_lgl(result, is.data.frame)
  any_local <- any(is_local)
  any_remote <- any(!is_local)

  if (!force_proceed & any_remote & any_local) {
    proceed <- readline(prompt = "Remote tables have to be collected (may be slow) in order to be binded. Proceed? [y/n]")

    if (proceed == "n") stop("\n Cancel by user. Try supply data from the same source (i.e., either all local or all remote).\n")
  }

  if (!any_local) {
    # if the data are all remote, do union in SQL;
    # union_all not necessary as already labeled by def and src; rows would not collapse across srcs
    result <- rlang::try_fetch(purrr::reduce(result, dplyr::union),
                               error = function(cnd) {
                                 rlang::warn("Returned unbinded result. Binding failed probably due to combining tables from different databases, which cannot be binded without collecting. Use force_collect = TRUE. Actual error message:\n", parent = cnd)
                                 return(result)
                               }
    )
    # manual return here to simplify the subsequent if logic
    return(result)
  }

  if (any_remote) {
    # if not all remote, also collect the remote ones before binding
    result <- purrr::map_if(result, !is_local, dplyr::collect, .progress = TRUE)
  }

  #dplyr::bind_rows(result, .id = "src_id")
  result <- rlang::try_fetch(purrr::list_rbind(result, names_to = "src_No") %>% dplyr::distinct(),
                             error = function(cnd) {
                               rlang::warn("Returned unbinded result. Binding failed probably due to incompatible types of the same variable from different sources. Actual error message:\n", parent = cnd)
                               return(result)
                             }
  )

  return(result)
}
