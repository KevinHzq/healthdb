#' Removes groups failed to meet conditions based on dates
#'
#' @description
#' In a vector of dates `date_var`, interpret if there could be at least one set of n elements taken from the date vector satisfy that adjacent elements in the set are at least certain days apart AND the dates in the set are within the specified time span. When identifying events/diseases from administrative data, definitions often require, e.g., n diagnoses that are at least some days apart within some years. This function is intended for such use and optimized to avoid looping through all n-size combinations.
#'
#'
#' @param data Data frames or remote tables (e.g., from `vignette("dbplyr", package = "dbplyr)`)
#' @param clnt_id Grouping variable (quoted/unquoted).
#' @param date_var Variable name (quoted/unquoted) for the dates to be interpreted.
#' @param n An integer for the size of a draw.
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param uid Variable name for a unique row identifier. It is necessary for SQL to produce consistent result based on sorting.
#' @param mode Either "flag" - add a new column 'flag_restrict_dates' indicating if a record met the condition (flag = 1 if the period starting/ending at the current record satisfied the apart-within condition), or "filter" - remove clients without any qualified record from the data. Default is "flag".
#' @param align Character, define if the time span for each record should start ("left") or end ("right") at its current date. Defaults to "left". Note that this would impact the first/last qualified dates, e.g., using "right" will have the first flag = 1 not at the earliest date. For example, if the condition was 2 records within a year, for c("2023-01-01", "2023-04-01", "2024-05-01"), flag will be c(0, 1, 0) for "right" while c(1,0,0) for "left".
#' @param dup.rm Logical for whether duplicated dates should be removed before calculation. Default is TRUE.
#' @param force_collect A logical for whether force downloading remote table if `apart` is not NULL. For remote table only, because `apart` is implemented for local data frame only. Downloading data could be slow, so the user has to opt in; default FALSE will stop with error.
#' @param verbose A logical for whether to explain the query and report how many groups were removed. Default is fetching from options. Use options(odcfun.verbose = FALSE) to suppress once and for all. Reporting is not for remote tables as the query is not executed immediately, thus no result is available for summary without adding an extra run (may be slow) of the query.
#' @param ... Additional argument passing to [data.table::as.IDate()] for date conversion.
#' @seealso [if_dates()]
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
#' restrict_dates(df, clnt_id, service_dt, n = 2, apart = 7, within = 30)
restrict_dates <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, uid = NULL, mode = c("flag", "filter"), align = c("left", "right"), dup.rm = TRUE, force_collect = FALSE, verbose = getOption("odcfun.verbose")
, ...) {
  rlang::check_required(clnt_id)
  rlang::check_required(date_var)
  UseMethod("restrict_dates")
}
