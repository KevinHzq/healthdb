#' @export
collapse_episode.data.frame <- function(data, clnt_id, start_dt, end_dt = NULL, gap, overwrite = NULL, gap_overwrite = 99999, .dt_trans = data.table::as.IDate, ...) {
  # as_name(enquo(arg)) converts both quoted and unquoted column name to string
  clnt_id_nm <- rlang::as_name(rlang::enquo(clnt_id))
  start_dt_nm <- rlang::as_name(rlang::enquo(start_dt))
  has_end <- !rlang::quo_is_null(rlang::enquo(end_dt))
  has_overwrite <- !rlang::quo_is_null(rlang::enquo(overwrite))
  if (has_end) {
    end_dt_nm <- rlang::as_name(rlang::enquo(end_dt))
  } else {
    end_dt_nm <- NULL
  }
  if (has_overwrite) {
    overwrite_nm <- rlang::as_name(rlang::enquo(overwrite))
  } else {
    overwrite_nm <- NULL
  }

  # place holder for temp column names
  latest_end_dt <- epi_id <- epi_no <- epi_seq <- last_end_dt <- last_overwrite <- scenario <- NULL

  data <- data.table::as.data.table(data)

  # error on name conflicts (consistent with the database method)
  temp_cols <- c("last_end_dt", "scenario", "latest_end_dt")
  new_cols <- c("epi_id", "epi_no", "epi_seq", "epi_start_dt", "epi_stop_dt")
  if (any(colnames(data) %in% c(temp_cols, new_cols))) stop(paste("Existing variable names conflict those that will be used to derive episodes. Please rename or remove any of ", stringr::str_flatten_comma(c(temp_cols, new_cols))))

  # date transform
  if (!is.null(.dt_trans)) {
    data[, c(start_dt_nm, end_dt_nm) := lapply(.SD, function(x) .dt_trans(x, ...)), .SDcols = c(start_dt_nm, end_dt_nm)]
  }

  # drop records with missing dates before deriving gaps; an NA date makes the
  # gap undefined and would otherwise propagate through cummax() and silently
  # merge every later record into the same episode. Mirrors restrict_dates().
  is_missing <- is.na(data[[start_dt_nm]])
  if (has_end) is_missing <- is_missing | is.na(data[[end_dt_nm]])
  n_missing <- sum(is_missing)
  if (n_missing > 0) {
    warning("Removed ", n_missing, " records with missing start_dt/end_dt")
    data <- data[!is_missing]
  }

  # if end date was not supplied, treat it as the same as start
  if (!has_end) {
    end_dt_nm <- "temp_end"
    temp_cols <- c(temp_cols, end_dt_nm)
    data[, c(end_dt_nm) := .SD, .SDcols = start_dt_nm]
  } else {
    # check start <= end?
    err <- data[data[[start_dt_nm]] > data[[end_dt_nm]]]
    if (nrow(err) > 0) {
      warning("Potential error where start > end in the following rows:")
      print(err)
    }
  }

  # sort data
  data.table::setorderv(data, cols = c(clnt_id_nm, start_dt_nm, end_dt_nm, overwrite_nm))

  # The complication in the collapsing logic is that consecutive rows may not be consecutive in time and could overlap in different ways. The following logic is to compare the current start date with the latest (not necessarily the previous) end date, then label the scenarios and assign episode number based on the scenario indicator.

  # if use overwrite, change the scenario values
  if (has_overwrite) {
    # get last end date/overwrite for each row
    data[, (paste0("last_", c("end_dt", "overwrite"))) := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = c(end_dt_nm, overwrite_nm)]
    # get cumulative maximum of expiry date up to the previous row
    data[, latest_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]

    data[, scenario := data.table::fifelse(last_overwrite != .SD[[overwrite_nm]], .SD[[start_dt_nm]] > (latest_end_dt + gap), .SD[[start_dt_nm]] > (latest_end_dt + gap_overwrite))]

    data[, last_overwrite := NULL]
  } else {
    # get last end date/overwrite for each row
    data[, last_end_dt := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = end_dt_nm]
    # get cumulative maximum of expiry date up to the previous row
    data[, latest_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]

    data[, scenario := data.table::fcase(.SD[[start_dt_nm]] > (latest_end_dt + gap), 1,
      default = 0
    )]
  }

  # increment epi_no (within person) at scenario = 0 where the episode started
  data[, epi_no := cumsum(scenario) + 1, by = clnt_id_nm]
  # give each episode unique id in the whole data
  data[, epi_id := .GRP, by = c(clnt_id_nm, "epi_no")]
  # create seq# within an episode for each row
  data[, epi_seq := data.table::rowidv(epi_no), by = clnt_id_nm]
  # summarize start and end date for each episode; guard the empty case so
  # min()/max() do not warn ("no non-missing arguments") on zero-row input
  if (nrow(data) > 0L) {
    data[, `:=`(
      epi_start_dt = min(.SD[[start_dt_nm]], na.rm = TRUE),
      epi_stop_dt = max(.SD[[end_dt_nm]], na.rm = TRUE)
    ),
    by = list(data[[clnt_id_nm]], epi_no), .SDcols = c(start_dt_nm, end_dt_nm)
    ]
  } else {
    # add the summary columns with the correct (empty) date types
    data[, `:=`(epi_start_dt = data[[start_dt_nm]], epi_stop_dt = data[[end_dt_nm]])]
  }
  # clean up aux variables
  data[, c(temp_cols) := NULL]

  data.table::setcolorder(data, c(setdiff(names(data), new_cols), new_cols))

  data.table::setDF(data)

  return(data)
}
