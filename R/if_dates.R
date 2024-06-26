#' Interpret if any n elements drawn from a date vector could be some days apart within some time span
#'
#' @md
#' @description
#' Given a vector of dates x, interpret if there could be at least one set of n elements taken from x satisfy that adjacent elements in the set are at least certain days apart AND the dates in the set are within the specified time span. When identifying events/diseases from administrative data, definitions often require, e.g., n diagnoses that are at least some days apart within some years. This function is intended for such use and optimized to avoid looping through all n-size combinations in x. This function does not work with remote table input.
#'
#' @param x A character or Date vector
#' @param n An integer for the size of a draw
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param detail Logical for whether return result per element of x.The default is FALSE, which returns one logical summarized by any(). Detail is not available if `apart` was supplied without `within` because sets that satisfied the condition could overlap, and records within a set may be far apart; thus, no unambiguous way to label by element.
#' @param align Character, define if the time span for each record should start ("left") or end ("right") at its current date. Defaults to "left". See 'flag_at' argument in [restrict_date()] for detail.
#' @param dup.rm Logical for whether multiple records on the same date should be count as one in calculation. Only applicable when `within` is supplied without `apart`; duplicated dates have no impact when `apart` is present as the n dates must be distinct if they were apart. Default is TRUE.
#' @param ... Additional argument passing to [data.table::as.IDate()] for date conversion.
#' @seealso [restrict_date()]
#'
#' @return Single or a vector of logical for whether there is any draw from x satisfied the conditions
#' @export
#'
#' @examples
#' dates_of_records <- sample(seq(as.Date("2015-01-01"), as.Date("2021-12-31"), 7), 10)
#'
#' # whether there is any 3 records at least 30 days apart within 2 years
#' if_date(dates_of_records, n = 3, apart = 30, within = 365 * 2)
#'
#' # specified either apart or within or both
#' if_date(dates_of_records, n = 2, within = 365)
#'
if_date <- function(x, n, apart = NULL, within = NULL, detail = FALSE, align = c("left", "right"), dup.rm = TRUE, ...) {
  if (all(is.null(apart), is.null(within))) stop("apart and within cannot both be NULL")

  # stopifnot("x must be character or Date" = any(is.character(x), lubridate::is.Date(x)))
  stopifnot(n > 1, is.wholenumber(n))
  align <- rlang::arg_match0(align, c("left", "right"))

  # place holder for var names
  N <- len_enough <- period_id <- real_end <- start <- d <- y <- NULL

  len <- length(x)

  if (len < n) {
    return(FALSE)
  }

  x <- data.table::as.IDate(x, ...)
  # for sorting result back to the order corresponding to the original x
  # https://stackoverflow.com/questions/15464793/restoring-original-order-of-a-vector-matrix-in-r
  ord <- order(x)
  x <- x[ord]

  # just interpret apart if no within. see all_part.R for detail
  if (is.null(within)) {
    stopifnot(is.wholenumber(apart))
    return(all_apart(x, n, apart))
  }

  if (is.null(apart)) {
    # if just within, the adjacent n has the smallest gaps, if none of the rolling window < within, no other combinations will be so.
    stopifnot(is.wholenumber(within))

    if (dup.rm) {
      x0 <- x
      x <- unique(x)
    }

    x_roll <- data.table::frollapply(x = x, n = n, align = align, FUN = function(x) sum(diff(x)) <= within)
    dtx <- data.table::data.table(x = as.numeric(x_roll == 1))
    data.table::setnafill(dtx, fill = 0)

    if (detail) {
      if (dup.rm) {
        # fill detail vector when there are duplicates
        x_roll <- dtx[, x]
        for (i in x0[duplicated(x0)]) {
          x_roll <- append(x_roll, NA, which(x == i))
          x <- append(x, NA, which(x == i))
        }
        x_roll <- data.table::nafill(x_roll, type = "locf")
        return(as.logical(x_roll)[order(ord)])
      } else {
        return(as.logical(dtx[, x])[order(ord)])
      }
    } else {
      return(any(as.logical(dtx[, x]), na.rm = TRUE))
    }
  } else {
    stopifnot(is.wholenumber(c(apart, within)))
    if (apart * (n - 1) >= within) stop("Condition is impossible as apart*(n - 1) cannot be greater than within")
    # overlap join dates and dates+within to get records falls in every within window starting at each date. This ensure the within condition, then calls all_apart to test the apart condition
    # tested against combn(sample, n, function(x) all(diff(sort(x)) >= m) & (diff(c(min(x), max(x))) <= within)) %>% any()
    # browser()
    dtx <- data.table::data.table(d = x, y = x, key = c("d", "y"))

    switch(align,
      left = {
        dty <- data.table::data.table(start = x, end = x + within, period_id = seq(1:len), key = c("start", "end"))
      },
      right = {
        dty <- data.table::data.table(start = x - within, end = x, period_id = seq(1:len), key = c("start", "end"))
      }
    )

    overlap <- data.table::foverlaps(dtx, dty)[, y := NULL]
    # browser()
    data.table::setorder(overlap, period_id, d)

    # filters with n for reducing compute
    if (!detail) {
      overlap[, N := .N, by = period_id]
      overlap <- overlap[N >= n]
      if (nrow(overlap) == 0) {
        return(FALSE)
      }
    }
    # intended to reduce compute but need to adjust for `align`
    # overlap[, real_end := data.table::last(d), by = period_id]
    # if (!detail) overlap <- overlap[, len_enough := (real_end - start) >= apart * (n - 1)][len_enough == TRUE]

    overlap <- overlap[, list(incl = all_apart(d, n, apart)), by = period_id]

    if (detail) {
      return(overlap[["incl"]][order(ord)])
    } else {
      return(any(overlap[["incl"]], na.rm = TRUE))
    }
  }
}

if_dates <- if_date
