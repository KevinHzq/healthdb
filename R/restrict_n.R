#' Removes groups with n less than some number
#'
#' @description
#' In a grouped data frame or remote table, remove groups that have less than some number of rows or some number of distinct values in a variable. For example, it can be used to remove clients that had less than n visits to some service on different dates from some administrative records.
#'
#' @param data Data frames or remote tables (e.g., from dbplyr)
#' @param clnt_id Grouping variable (quoted/unquoted). The default is fetching "clnt_id" from the data's attributes.
#' @param n_per_clnt A single number specifying the minimum number of group size.
#' @param count_by Another variable dictating the counting unit of n_per_clnt. The default is NULL meaning the inclusion criteria is the number of row, i.e., dplyr::n() >= n_per_clnt. If it is not NULL, the criteria becomes dplyr::n_distinct(count_by) >= n_per_clnt.
#' @param verbose A logical for whether to report how many groups were removed.
#'
#' @return A subset of input data satisfied the group size requirement.
#' @export
#'
#' @examples
#' #remove cyl groups with less than 8 cars
#' restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 8)
#'
#' #remove cyl groups with less than 2 types of gear boxes
#' restrict_n(mtcars, clnt_id = cyl, n_per_clnt = 3, count_by = gear)
restrict_n <- function(data, clnt_id = NULL, n_per_clnt, count_by = NULL, verbose = TRUE) {
  stopifnot(is.numeric(n_per_clnt))
  UseMethod("restrict_n")
}



