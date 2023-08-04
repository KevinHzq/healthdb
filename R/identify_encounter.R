#' Title
#'
#' @param data
#' @param from_var
#' @param match_vals
#' @param match_type
#' @param if_all
#' @param verbose
#' @param query_only
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
identify_encounter <- function(data, from_var, match_vals, match_type = c("in", "start", "regex", "like", "between", "glue_sql"), if_all = FALSE, verbose = TRUE, query_only = TRUE, ...) {
  UseMethod("identify_encounter")
}
