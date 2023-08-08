#' Removes groups failed to meet conditions based on dates
#'
#' @description
#' In a vector of dates `date_var`, interpret if there could be at least one set of n elements taken from the date vector satisfy that adjacent elements in the set are at least certain days apart AND the dates in the set are within the specified time span. When identifying events/diseases from administrative data, definitions often require, e.g., n diagnoses that are at least some days apart within some years. This function is intended for such use and optimized to avoid looping through all n-size combinations. Dates are interpret within `clnt_id` groups, and groups failed to meet the conditions are removed from the data.
#'
#'
#' @param data Data frames or remote tables (e.g., from dbplyr)
#' @param clnt_id Grouping variable (quoted/unquoted).
#' @param date_var Variable name (quoted/unquoted) for the dates to be interpreted.
#' @param n An integer for the size of a draw.
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param dup.rm Logical for whether duplicated dates in x should be removed before calculation. Default is TRUE.
#' @param force_collect A logical for whether force downloading remote table if `apart` is not NULL. For remote table only, because `apart` is implemented for local data frame only. Downloading data could be slow, so the user has to opt in; default FALSE will stop with error.
#' @param verbose A logical for whether to report how many groups were removed.
#' @param ... Additional argument passing to data.table::as.IDate for date conversion.
#'
#' @return A subset of input data satisfied the dates requirement.
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
restrict_dates <- function(data, clnt_id, date_var, n, apart = NULL, within = NULL, dup.rm = TRUE, force_collect = FALSE, verbose = TRUE, ...) {
  UseMethod("restrict_dates")
}
