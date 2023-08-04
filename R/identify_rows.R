#' Title
#'
#' @param data
#' @param vars
#' @param match
#' @param vals
#' @param if_all
#' @param verbose
#' @param query_only
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
identify_rows <- function(data, vars, match = c("in", "start", "regex", "like", "between", "glue_sql"), vals, if_all = FALSE, verbose = TRUE, query_only = TRUE, ...) {
  UseMethod("identify_rows")
}
