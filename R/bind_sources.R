#' Row-bind a list of data.frames into one
#'
#' @param data A list of data.frame.
#' @param ... Variables in the output. The argument name should be the new name in the output, and the right hand side of argument is a character vector of the original names. The name vector and the data will be matched by position. if an output variable only came from some of the sources, fill the name vector to a length equal to the number of sources with NA, e.g., 'var' only come from the second out of three sources, var = c(NA, 'nm_in_src2', NA).
#'
#' @return A data.frame contains combined rows of the input list with variables specified by ...
#' @export
#'
#' @examples
#' df1 <- subset(iris, Species == "setosa")
#' df2 <- subset(iris, Species == "versicolor")
#' df3 <- subset(iris, Species == "virginica")
#'
#' bind_sources(list(df1, df2, df3),
#'  s_l = "Sepal.Length",
#'  s_w = "Sepal.Width",
#'  p_l_setosa = c("Petal.Length", NA, NA),
#'  p_l_virginica = c(NA, NA, "Petal.Length")
#' )
bind_sources <- function(data, ...) {
  # capture data names in the original env before any eval
  data_quo <- rlang::enquo(data)
  data_expr <- data_quo %>% rlang::call_args()
  data_env <- data_quo %>% rlang::quo_get_env()
  n_data <- length(data_expr)

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
  dplyr::bind_rows(result, .id = "src_id")
}
