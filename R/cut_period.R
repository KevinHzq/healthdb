#' Cut the time period in one row into multiple rows by interval
#'
#' @description This function is useful for cutting time ranges into levels for overlap joins.
#' @param dat Input dataframe that each row has start and end dates
#' @param start Record start date column (unquoted)
#' @param end Record end date column (unquoted)
#' @param len An integer, the interval that would be used to divide the record duration
#' @param unit One of "day" (default), "week", "month", "quarter, or "year" used in combination of len to specify the time length of the interval.
#' @param .dt_trans Function to transform start/end, such as lubridate::ymd. Default is NULL.
#'
#' @return Data frame that each row is now a segment of the period defined by (start, end) in the original row. Original variables are retained and repeated for each segment plus new variables defining the segment interval.
#'
#' @export
#'
#' @examples
#' # toy data
#' df <- data.frame(sample_id = 1, period_id = 1, start_date = "2015-01-01", end_date = "2019-12-31")
#'
#' # divide period into segments (multiple rows per period)
#' df_seg <- cut_period(
#'   dat = df, start = start_date, end = end_date,
#'   len = 30, .dt_trans = lubridate::ymd
#' )
#'
#' # categorize segment_id as factor
#' df_seg$segment <- cut(df_seg$segment_id,
#'   breaks = c(0, 1, 2, Inf),
#'   labels = c("< 1 month", "1 - 2 months", "Remainder")
#' )
cut_period <- function(dat, start, end, len, unit = c("day", "week", "month", "quarter", "year"), .dt_trans = NULL) {
  # input checks
  unit <- rlang::arg_match0(unit, c("day", "week", "month", "quarter", "year"))

  new_cols <- c("segment_start", "segment_end", "segment_id")
  dat <- dat %>% dplyr::rename_with(~ paste(.x, "og", sep = "_", recycle0 = TRUE), dplyr::any_of(new_cols))

  if (!is.null(.dt_trans)) {
    dat <- dat %>% dplyr::mutate(dplyr::across(c({{ start }}, {{ end }}), .dt_trans))
  } else if (any(!lubridate::is.Date(dplyr::pull(dat, {{ start }})), !lubridate::is.Date(dplyr::pull(dat, {{ end }})))) stop("The class of start/end is not Date. Supply a function such as lubridate::ymd with the .dt_trans argument to transform.")

  dat %>%
    dplyr::mutate(
      segment_start = purrr::map2({{ start }}, {{ end }}, ~ seq(.x, .y, by = paste(len, unit))),
      segment_end = purrr::map2(.data$segment_start, {{ end }}, ~ .x %>% dplyr::lead(, default = .y + lubridate::days(1)) - lubridate::days(1)),
      segment_id = purrr::map(.data$segment_start, ~ seq_along(lengths(.x)))
    ) %>%
    tidyr::unnest(cols = dplyr::any_of(new_cols))
}
