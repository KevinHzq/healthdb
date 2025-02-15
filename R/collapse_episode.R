#' Group records no more than n days apart as episodes
#'
#' @md
#' @description This function is useful for collapsing, e.g., medication dispensation or hospitalization, records into episodes if the records' dates are no more than n days gap apart. The length of the gap can be relaxed by another grouping variable.
#' @param data A data.frame or remote table that contains the id and date variables.
#' @param clnt_id Column name of subject/person ID.
#' @param start_dt Column name of the starting date of records.
#' @param end_dt Column name of the end date of records. The default is NULL assuming the record last one day and only the start date will be used to calculate the gaps between records.
#' @param gap A number in days that will be used to separate episodes. For example, gap = 7 means collapsing records no more than 7 days apart. Note that the number of days apart will be calculated as numeric difference between two days, so that 2020-01-07 and 2020-01-01 is considered as 6 days apart.
#' @param overwrite Column name of a grouping variable determining whether the consecutive records are related and should have a different gap value. For example, dispensing records may have the same original prescription number, and a different gap value can be assigned for such situation, e.g., the days between two records is > gap, but these records still belong to the same prescription.
#' @param gap_overwrite A different gap value used for related records. The default is 99999, which practically means all records with the same overwrite variable will be collapsed.
#' @param .dt_trans Function to transform start_dt/end_dt. For data.frame input only. Default is [data.table::as.IDate()].
#' @param ... Additional arguments passing to the .dt_trans function. For data.frame input only.
#'
#' @return The original data.frame or remote table with new columns indicating episode grouping. The new variables include:
#' - epi_id: unique identifier of episodes across the whole data set
#' - epi_no: identifier of episodes within a client/group
#' - epi_seq: identifier of records within an episode
#' - epi_start/stop_dt: start and end dates corresponding to epi_id
#' @export
#'
#' @examples
#' # make toy data
#' df <- make_test_dat() %>%
#' dplyr::select(clnt_id, dates)
#'
#' head(df)
#'
#' # collapse records no more than 90 days apart
#' # end_dt could be absent then it is assumed to be the same as start_dt
#' collapse_episode(df, clnt_id, start_dt = dates, gap = 90)
collapse_episode <- function(data, clnt_id, start_dt, end_dt = NULL, gap, overwrite = NULL, gap_overwrite = 99999, .dt_trans = data.table::as.IDate, ...) {
  # input checks

  if (any(!is.numeric(gap), !is.numeric(gap_overwrite))) stop("gap/gap_overwrite must be numeric.")
  UseMethod("collapse_episode")
}
