collapse_episode <- function(dat, start_dt_nm, end_dt_nm = NULL, gap, overwrite_nm = NULL, gap_overwrite = Inf, clnt_id_nm, .dt_trans = data.table::as.IDate, ...) {
  # input checks
  if (any(sapply(list(clnt_id_nm, start_dt_nm, end_dt_nm, overwrite_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

  if (any(!is.numeric(gap), !is.numeric(gap_overwrite))) stop("gap/gap_overwrite must be numeric.")

  # place holder for temp column names
  gap_days <- epi_end_dt <- epi_id <- epi_no <- epi_seq_no <- ind <- last_end_dt <- last_overwrite <- scenario <- NULL

  dat <- data.table::as.data.table(dat)

  # treat potential name conflicts
  temp_cols <- c("last_end_dt", "scenario", "ind", "epi_end_dt")
  new_cols <- c("gap_days", "epi_id", "epi_no", "epi_seq_no", "epi_start_dt", "epi_stop_dt")
  data.table::setnames(dat, old = c(new_cols, temp_cols), new = paste(c(new_cols, temp_cols), "og", sep = "_", recycle0 = TRUE), skip_absent = TRUE)

  # date transform
  if (!is.null(.dt_trans)) {
    dat[, c(start_dt_nm, end_dt_nm) := lapply(.SD, function(x) data.table::as.IDate(x, ...)), .SDcols = c(start_dt_nm, end_dt_nm)]
  }

  # if end date was not supplied, treat it as the same as start
  if (is.null(end_dt_nm)) {
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

  # The complication in the collapsing logic is that consecutive rows may not be consecutive in time and could overlap in different ways. The following logic is two compare the current start date with the latest (not necessarily the previous) end date, then differentiate the scenarios and assign episode number based on the scenario indicator.

  # if use overwrite, change the scenario values
  if (!is.null(overwrite_nm)) {
    # get last end date/overwrite for each row
    dat[, (paste0("last_", c("end_dt", "overwrite"))) := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = c(end_dt_nm, overwrite_nm)]
    # row id within person
    dat[, ind := data.table::rowidv(.SD), .SDcols = clnt_id_nm]
    # get cumulative maximum of expiry date up to the previous row
    dat[, epi_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]
    # compute difference between current start date and cumulative maximum of end date
    dat[, gap_days := data.table::fcase(
      ind != 1, sapply(.SD, function(x) as.numeric(x - epi_end_dt)),
      default = 0
    ),
    by = clnt_id_nm, .SDcols = start_dt_nm
    ]
    # interpret scenario of overlapping; no overlap -> NA
    dat[, scenario := data.table::fifelse(
      ind != 1,
      data.table::fcase( # if not the person's first record, interpret, = 0 otherwise
        gap_days < 0 & epi_end_dt < .SD[[end_dt_nm]], 1L, # overlaps the last record and end date > previous end - collapse
        gap_days < 0 & epi_end_dt >= .SD[[end_dt_nm]], 2L, # last record contains the current one - collapse
        gap_days >= 0 & gap_days < gap, 3L, # no overlap but within allowed gap spec - collapse
        gap_days >= 0 & gap_days >= gap & identical(last_overwrite, .SD[[overwrite_nm]]) & gap_days < gap_overwrite, 4L # exceed allowed gap spec but overwritten - collapse
      ), 0L
    ),
    by = clnt_id_nm, .SDcols = c(end_dt_nm, overwrite_nm)
    ]

    dat[, last_overwrite := NULL]
  } else {
    # get last end date/overwrite for each row
    dat[, last_end_dt := lapply(.SD, function(x) data.table::shift(x, n = 1, fill = data.table::first(x))), by = clnt_id_nm, .SDcols = end_dt_nm]
    # row id within person
    dat[, ind := data.table::rowidv(.SD), .SDcols = clnt_id_nm]
    # get cumulative maximum of expiry date up to the previous row
    dat[, epi_end_dt := cummax(as.integer(last_end_dt)) %>% data.table::as.IDate(), by = clnt_id_nm]
    # compute difference between current start date and cumulative maximum of end date
    dat[, gap_days := data.table::fcase(
      ind != 1, sapply(.SD, function(x) as.numeric(x - epi_end_dt)),
      default = 0
    ),
    by = clnt_id_nm, .SDcols = start_dt_nm
    ]
    # interpret scenario of overlapping; no overlap -> NA
    dat[, scenario := data.table::fifelse(
      ind != 1,
      data.table::fcase( # if not the person's first record, interpret, = 0 otherwise
        gap_days < 0 & epi_end_dt < .SD[[end_dt_nm]], 1L, # overlaps the last record and end date > previous end - collapse
        gap_days < 0 & epi_end_dt >= .SD[[end_dt_nm]], 2L, # last record contains the current one - collapse
        gap_days >= 0 & gap_days < gap, 3L # no overlap but within allowed gap spec - collapse
      ), 0L
    ),
    by = clnt_id_nm, .SDcols = c(end_dt_nm, overwrite_nm)
    ]
  }

  # fill the non-overlapping scenario with 0 which means epi start
  data.table::setnafill(dat, cols = "scenario", fill = 0)
  # increment epi_no (within person) at scenario = 0 where the episode started
  dat[, epi_no := cumsum(scenario == 0), by = clnt_id_nm]
  # give each episode unique id in the whole data
  dat[, epi_id := data.table::rleid(epi_no)]
  # create seq# within an episode for each row
  dat[, epi_seq_no := data.table::rowidv(epi_no), by = clnt_id_nm]
  # summarize start and end date for each episode
  dat[, `:=`(
    epi_start_dt = data.table::first(.SD[[start_dt_nm]]),
    epi_stop_dt = data.table::last(.SD[[end_dt_nm]])
  ),
  by = list(dat[[clnt_id_nm]], epi_no), .SDcols = c(start_dt_nm, end_dt_nm)
  ]
  # clean up aux variables
  dat[, c(temp_cols) := NULL]

  data.table::setcolorder(dat, c(setdiff(names(dat), new_cols), new_cols))

  data.table::setDF(dat)

  return(dat)
}
