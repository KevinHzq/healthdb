#' @export
collapse_episode.tbl_sql <- function(data, clnt_id, start_dt, end_dt = NULL, gap, overwrite = NULL, gap_overwrite = 99999, .dt_trans = data.table::as.IDate, ...) {
  # input checks
  # if (any(sapply(list(clnt_id_nm, start_dt_nm, end_dt_nm, overwrite_nm), function(x) !is.null(x) & !is.character(x)))) stop("Arguments ended with _nm must be characters.")

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

  gap <- as.integer(gap)
  gap_overwrite <- as.integer(gap_overwrite)

  # place holder for temp column names
  latest_end_dt <- epi_id <- epi_no <- epi_seq <- last_end_dt <- last_overwrite <- scenario <- NULL

  # treat potential name conflicts
  temp_cols <- c("last_end_dt", "scenario", "latest_end_dt")
  new_cols <- c("epi_id", "epi_no", "epi_seq", "epi_start_dt", "epi_stop_dt")
  if (any(colnames(data) %in% c(temp_cols, new_cols))) stop(paste("Existing variable names conflict those that will be used to derive episodes. Please rename or remove any of ", stringr::str_flatten_comma(c(temp_cols, new_cols))))

  # if end date was not supplied, treat it as the same as start
  if (!has_end) {
    end_dt_nm <- "temp_end"
    temp_cols <- c(temp_cols, end_dt_nm)
    data <- data %>%
      dplyr::mutate(temp_end = .data[[start_dt_nm]])
  }

  # sort data
  data <- data %>%
    dplyr::group_by(.data[[clnt_id_nm]]) %>%
    dbplyr::window_order(dplyr::pick(dplyr::any_of(c(start_dt_nm, end_dt_nm, overwrite_nm)))) %>%
    dplyr::mutate(
      last_end_dt = dplyr::coalesce(dplyr::lag(.data[[end_dt_nm]], 1L), .data[[end_dt_nm]]),
      latest_end_dt = cummax(last_end_dt)
    )

  # The complication in the collapsing logic is that consecutive rows may not be consecutive in time and could overlap in different ways. The following logic is to compare the current start date with the latest (not necessarily the previous) end date, then label the scenarios and assign episode number based on the scenario indicator.

  # if use overwrite, change the scenario values
  if (has_overwrite) {
    # get last end date/overwrite for each row
    data <- data %>%
      dplyr::mutate(last_overwrite = dplyr::lag(.data[[overwrite_nm]], 1L))
    # get cumulative maximum of expiry date up to the previous row
    data <- data %>%
      dplyr::mutate(scenario = dplyr::case_when(last_overwrite != .data[[overwrite_nm]] & .data[[start_dt_nm]] > (latest_end_dt + local(gap)) ~ 1L,
        last_overwrite == .data[[overwrite_nm]] & .data[[start_dt_nm]] > (latest_end_dt + local(gap_overwrite)) ~ 1L,
        .default = 0L
      ))

    temp_cols <- c(temp_cols, "last_overwrite")
  } else {
    data <- data %>%
      dplyr::mutate(scenario = dplyr::case_when(.data[[start_dt_nm]] > (latest_end_dt + local(gap)) ~ 1L,
        .default = 0L
      ))
  }


  # increment epi_no (within person) at scenario = 0 where the episode started
  data <- data %>%
    dplyr::mutate(epi_no = cumsum(scenario) + 1L)


  # create seq# within an episode for each row
  # summarize start and end date for each episode
  data <- data %>%
    dplyr::group_by(.data[[clnt_id_nm]], epi_no) %>%
    dbplyr::window_order(dplyr::pick(dplyr::any_of(c(start_dt_nm, end_dt_nm, overwrite_nm)))) %>%
    dplyr::mutate(
      epi_seq = dplyr::row_number(),
      epi_start_dt = min(.data[[start_dt_nm]], na.rm = TRUE),
      epi_stop_dt = max(.data[[end_dt_nm]], na.rm = TRUE)
    )

  # give each episode unique global id
  data <- data %>%
    dplyr::ungroup() %>%
    dbplyr::window_order(.data[[clnt_id_nm]], epi_no) %>%
    dplyr::mutate(epi_id = dplyr::dense_rank())


  # clean up aux variables
  keep_cols <- c(dplyr::setdiff(colnames(data), new_cols))

  data <- data %>%
    dplyr::select(dplyr::any_of(c(keep_cols, new_cols)), -dplyr::any_of(temp_cols))

  return(data)
}
