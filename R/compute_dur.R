#' Title Compute duration between two dates
#'
#' @description
#' This function is meant to be used with [dplyr::mutate()] to compute age/duration between two character or Date columns.
#'
#' @param from A character or Date vector for start dates.
#' @param to A character or Date vector for end dates.
#' @param lower_brks A numeric vector for lower breaks passing to the base [cut()] function to convert the numeric result to a factor. The level will be auto generated. For example, the level labels are c("<19", "19-24", "25-34", "35-44", "45-54", "55+") for lower_brks = c(0, 19, 25, 35, 45, 55). Default is NULL (no conversion).
#' @param unit A character string specifying the unit of the output. One of "year" (default), "day", "week", or "month".
#' @param trans A logical for whether transform both `from` and `to` with the `.transfn` function
#' @param .transfn A function for transforming the inputs. Default is [lubridate::ymd()].
#' @param verbose A logical for whether print summary of the out and warning for missing values.
#' @param ... Additional arguments passing to [cut()].
#'
#' @return A numeric or factor vector of the duration.
#' @export
#'
#' @examples
#' # toy data
#' n <- 5
#' df <- data.frame(id = 1:n,
#' start_dt = sample(seq(as.Date("1970-01-01"), as.Date("2000-12-31"), by = 1), size = n),
#' end_dt = sample(seq(as.Date("2001-01-01"), as.Date("2023-12-31"), by = 1), size = n))
#'
#' # get age group at a cut-off
#' df %>% dplyr::mutate(
#'  age_grp = compute_dur(start_dt, "2023-01-01", lower_brks = c(0, 19, 25, 35, 45, 55))
#' )
#'
#' # compute gaps between two dates in weeks
#' df %>% dplyr::mutate(
#'  gap_wks = compute_dur(start_dt, end_dt, unit = "week")
#' )
compute_dur <- function(from, to, lower_brks = NULL, unit = c("year", "day", "week", "month"), trans = FALSE, .transfn = lubridate::ymd, verbose = TRUE, ...) {
  #compute age with lubridate functions (more accurate than /365.25) and built-in transformations
  unit <- rlang::arg_match0(unit, c("year", "day", "week", "month"))

  if (trans) {
    rlang::check_required(.transfn)
    stopifnot(rlang::is_function(.transfn))
    from <- .transfn(from)
    to <- .transfn(to)
  }

  age <- lubridate::interval(from, to) / lubridate::duration(1, units = unit)

  #warnings for unreasonable values
  if (verbose)
  {
    if(any(is.na(age))) warning("Output contains missing value(s)")
    print(summary(age))
  }

  if (!is.null(lower_brks)) {
    stopifnot(is.numeric(lower_brks))
    #create labels based on breaks
    labs <- sapply(1:length(lower_brks),
                   function(i) dplyr::case_when(i == 1 ~ paste0("<", lower_brks[i+1]),
                                                i == length(lower_brks) ~ paste0(lower_brks[i], "+"),
                                                .default = paste(lower_brks[i], lower_brks[i+1]-1, sep = "-")))

    #make age group with the lower boundary of breaks
    agegrp <- cut(age, breaks = c(lower_brks, Inf), labels = labs, right = FALSE, ...)

    #warning if the breaks don't cover the full range
    if (verbose) {
      if (sum(is.na(agegrp)) > sum(is.na(age))) warning("More NA than raw age. Breaks do not cover the full range.")
      print(summary(agegrp))
    }

    return(agegrp)

  }

  return(age)
}
