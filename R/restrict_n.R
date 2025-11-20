#' Remove or flag groups with n less than some number
#'
#' @md
#' @description
#' Remove or flags groups or clients that have less than some number of rows or some number of distinct values in a variable. For example, it can be used to remove clients that had less than n visits to some service on different dates from some administrative records. It offers filtering with [dplyr::n_distinct()] functionality for database input.
#'
#' @param data Data.frames or remote tables (e.g., from [dbplyr::tbl_sql()])
#' @param clnt_id Grouping variable (quoted/unquoted).
#' @param n_per_clnt A single number specifying the minimum number of group size.
#' @param count_by Another variable dictating the counting unit of `n_per_clnt.` The default is NULL meaning the inclusion criteria is the number of row, i.e., `dplyr::n() >= n_per_clnt`. If it is not NULL, the criteria becomes equivalent to `dplyr::n_distinct(count_by) >= n_per_clnt`.
#' @param mode Either "flag" - add a new column 'flag_restrict_n' indicating if the client met the condition (all rows from a qualified client would have flag = 1), or "filter" - remove clients that did not meet the condition from the data. Default is "flag".
#' @param verbose A logical for whether to explain the query and report how many groups were removed. Default is fetching from options. Use `options(healthdb.verbose = FALSE)` to suppress once and for all. Reporting is not for remote tables as the query is not executed immediately, thus no result is available for summary without adding an extra run (may be slow) of the query.
#' @seealso [dplyr::n()], [dplyr::n_distinct()]
#'
#' @return A subset of input data satisfied the group size requirement, or raw input data with an new flag column.
#' @export
#'
#' @examples
#' # flag cyl groups with less than 8 cars
#' restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 8, mode = "flag") %>%
#'   head()
#'
#' # remove cyl groups with less than 2 types of gear boxes
#' restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 3, count_by = gear, mode = "filter")
restrict_n <- function(data, clnt_id, n_per_clnt, count_by = NULL, mode = c("flag", "filter"), verbose = getOption("healthdb.verbose")) {
  rlang::check_required(clnt_id)
  stopifnot(is.numeric(n_per_clnt))
  UseMethod("restrict_n")
}
