#' Title Grouping records into episodes by date range
#'
#' @description This function is useful for collapsing, e.g., medication dispensation or hospitalization, records into episodes if the records' dates are no more than n days gap apart. The length of the gap can be relaxed by another grouping variable.
#' @param dat A data.frame that contains the id and date variables.
#' @param clnt_id Column name of subject/person ID.
#' @param start_dt Column name of the starting date of records.
#' @param end_dt Column name of the end date of records. The default is NULL assuming the record last one day and only the start date will be used to calculate the gaps between records.
#' @param gap A number in days that will be used to separate episodes. It should be the inclusive upper bound + 1. For example, gap = 8 means collapsing records no more than 7 days apart.
#' @param overwrite Column name of a grouping variable determining whether the consecutive records are related and should have a different gap value. For example, dispensing records may have the same original prescription number, and a different gap value can be assigned for situation, e.g., the days between two records is > gap, but these records still belong to the same prescription.
#' @param gap_overwrite A different gap value used for related records. The default is Inf, which means all records with the same overwrite variable will be collapsed.
#' @param .dt_trans Function to transform start_dt/end_dt. Default is [data.table::as.IDate()].
#' @param ... Additional arguments passing to the .dt_trans function.
#'
#' @return The original data.frame with new columns indicating episode grouping
#' @export
#'
#' @examples
#' # make toy data
#' df <- make_test_dat()
#'
#' head(df)
#'
#' # collapse records no more than 90 days apart
#' collapse_episode(df, clnt_id, dates, gap = 91)
collapse_episode <- function(dat, clnt_id, start_dt, end_dt = NULL, gap, overwrite = NULL, gap_overwrite = Inf, .dt_trans = data.table::as.IDate, ...) {
  # input checks
  # if (any(sapply(list(clnt_id_nm, start_dt_nm, end_dt_nm, overwrite_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id_nm <- rlang::as_name(rlang::enquo(clnt_id))
  start_dt_nm <- rlang::as_name(rlang::enquo(start_dt))
  has_end <- !rlang::quo_is_null(rlang::enquo(end_dt))
  has_overwrite <- !rlang::quo_is_null(rlang::enquo(overwrite))
  if (has_end) end_dt_nm <- rlang::as_name(rlang::enquo(end_dt))
  else end_dt_nm <- NULL
  if (has_overwrite) overwrite_nm <- rlang::as_name(rlang::enquo(overwrite))
  else overwrite_nm <- NULL

  if (any(!is.numeric(gap), !is.numeric(gap_overwrite))) stop("gap/gap_overwrite must be numeric.")

  # place holder for temp column names
  latest_end_dt <- epi_id <- epi_no <- epi_seq <- last_end_dt <- last_overwrite <- scenario <- NULL

  dat <- data.table::as.data.table(dat)

  # treat potential name conflicts
  temp_cols <- c("last_end_dt", "scenario", "latest_end_dt")
  new_cols <- c("epi_id", "epi_no", "epi_seq", "epi_start_dt", "epi_stop_dt")
  data.table::setnames(dat, old = c(new_cols, temp_cols), new = paste(c(new_cols, temp_cols), "og", sep = "_", recycle0 = TRUE), skip_absent = TRUE)

  # date transform
  if (!is.null(.dt_trans)) {
    dat[, c(start_dt_nm, end_dt_nm) := lapply(.SD, function(x) data.table::as.IDate(x, ...)), .SDcols = c(start_dt_nm, end_dt_nm)]
  }

  # if end date was not supplied, treat it as the same as start
  if (!has_end) {
    end_dt_nm <- "temp_end"
    temp_cols <- c(temp_cols, end_dt_nm)
    dat[, c(end_dt_nm) := .SD, .SDcols = start_dt_nm]
  } else {
    # check start <= end?
    err <- dat[dat[[start_dt_nm]] > dat[[end_dt_nm]]]
    if (nrow(err) > 0) {
      warning("Potential error where start > end in the following rows:")
      print(err)
    }
  }

  # sort data
  data.table::setorderv(dat, cols = c(clnt_id_nm, start_dt_nm, end_dt_nm, overwrite_nm))

  # The complication in the collapsing logic is that consecutive rows may not be consecutive in time and could overlap in different ways. The following logic is to compare the current start date with the latest (not necessarily the previous) end date, then label the scenarios and assign episode number based on the scenario indicator.

  # if use overwrite, change the scenario values
  if (has_overwrite) {
    # get last end date/overwrite for each row
    dat[, (paste0("last_", c("end_dt", "overwrite"))) := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = c(end_dt_nm, overwrite_nm)]
    # get cumulative maximum of expiry date up to the previous row
    dat[, latest_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]

    dat[, scenario := data.table::fifelse(last_overwrite != .SD[[overwrite_nm]], .SD[[start_dt_nm]] > (latest_end_dt + gap), .SD[[start_dt_nm]] > (latest_end_dt + gap_overwrite))]

    dat[, last_overwrite := NULL]
  }

  else {
    # get last end date/overwrite for each row
    dat[, last_end_dt := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = end_dt_nm]
    # get cumulative maximum of expiry date up to the previous row
    dat[, latest_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]

    dat[, scenario := data.table::fcase(.SD[[start_dt_nm]] > (latest_end_dt + gap), 1,
      default = 0
    )]
  }

  # increment epi_no (within person) at scenario = 0 where the episode started
  dat[, epi_no := cumsum(scenario) + 1, by = clnt_id_nm]
  # give each episode unique id in the whole data
  dat[, epi_id := .GRP, by = c(clnt_id_nm, "epi_no")]
  # create seq# within an episode for each row
  dat[, epi_seq := data.table::rowidv(epi_no), by = clnt_id_nm]
  # summarize start and end date for each episode
  dat[, `:=`(
    epi_start_dt = min(.SD[[start_dt_nm]], na.rm = TRUE),
    epi_stop_dt = max(.SD[[end_dt_nm]], na.rm = TRUE)
  ),
  by = list(dat[[clnt_id_nm]], epi_no), .SDcols = c(start_dt_nm, end_dt_nm)
  ]
  # clean up aux variables
  dat[, c(temp_cols) := NULL]

  data.table::setcolorder(dat, c(setdiff(names(dat), new_cols), new_cols))

  data.table::setDF(dat)

  return(dat)
}
