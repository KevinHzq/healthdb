#' Removes or flags groups failed to meet conditions based on dates
#'
#' @md
#' @description
#' For each client or group, interpret if they have n records that are at least certain days apart AND within a specified time span. When identifying events/diseases from administrative data, definitions often require, e.g., n diagnoses that are at least some days apart within some years. This function is intended for such use and optimized to avoid looping through all n-size combinations of dates per client.
#'
#'
#' @param data Data frames or remote tables (e.g., from [dbplyr::tbl_sql()])
#' @param clnt_id Grouping variable (quoted/unquoted).
#' @param date_var Variable name (quoted/unquoted) for the dates to be interpreted.
#' @param n An integer for the size of a draw.
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw. This option is only implemented for data.frame input.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param uid Variable name for a unique row identifier. It is necessary for SQL to produce consistent result based on sorting.
#' @param mode Either:
#' * "flag" - add a new column 'flag_restrict_date' indicating if the condition was met (flag = 1 if the time period starting or ending at the current record satisfied the apart-within condition),
#' * or "filter" - remove clients without any qualified record from the data. Default is "flag".
#' @param flag_at Character, define if the flag should be placed at the start ("left") or end ("right") of a time period that contains n qualified records. Defaults to "left". Note that this would impact the first and last qualified/diagnosed dates of a client, e.g., using "right" will have the first flag not at the earliest but the date which the client became qualified. For example, if the condition was 2 records within a year, for `c("2023-01-01", "2023-04-01", "2024-05-01")`, flag will be `c(0, 1, 0)` for "right" while `c(1,0,0)` for "left".
#' @param dup.rm Logical for whether multiple records on the same date should be count as one in calculation. Only applicable when `within` is supplied without `apart`; duplicated dates have no impact when `apart` is present as the n dates must be distinct if they were apart. Default is TRUE.
#' @param force_collect A logical for whether force downloading remote table if `apart` is not NULL. For remote table only, because `apart` is implemented for local data frame only. Downloading data could be slow, so the user has to opt in; default FALSE will stop with error.
#' @param verbose A logical for whether to explain the query and report how many groups were removed. Default is fetching from options. Use `options(healthdb.verbose = FALSE)` to suppress once and for all. Reporting is not for remote tables as the query is not executed immediately, thus no result is available for summary without adding an extra run (may be slow) of the query.
#' @param ... Additional argument passing to [data.table::as.IDate()] for date conversion.
#' @seealso [if_date()]
#'
#' @return A subset of input data satisfied the dates requirement, or raw input data with an new flag column.
#' @export
#'
#' @examples
#' sample_size <- 30
#' df <- data.frame(clnt_id = sample(1:sample_size, sample_size, replace = TRUE),
#'  service_dt = sample(seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = 1),
#'                      size = sample_size, replace = TRUE),
#'  diagx = sample(letters, size = sample_size, replace = TRUE),
#'  diagx_1 = sample(c(NA, letters), size = sample_size, replace = TRUE),
#'  diagx_2 = sample(c(NA, letters), size = sample_size, replace = TRUE))
#'
#' #Keep clients with 2 records that were 1 week apart within 1 month
#' restrict_date(df, clnt_id, service_dt, n = 2, apart = 7, within = 30)
restrict_date <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), flag_at = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("healthdb.verbose")
, ...) {
  rlang::check_required(clnt_id)
  rlang::check_required(date_var)
  UseMethod("restrict_dates")
}

restrict_dates <- restrict_date
