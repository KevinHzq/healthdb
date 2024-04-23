#' Interpret if n dates drawn from a vector could be some days apart within some years
#'
#' @md
#' @description
#' Given a vector of dates x, interpret if there could be at least one set of n elements taken from x satisfy that adjacent elements in the set are at least certain days apart AND the dates in the set are within the specified time span. When identifying events/diseases from administrative data, definitions often require, e.g., n diagnoses that are at least some days apart within some years. This function is intended for such use and optimized to avoid looping through all n-size combinations in x. This function does not work with remote table input.
#'
#' @param x A character or Date vector
#' @param n An integer for the size of a draw
#' @param apart An integer specifying the minimum gap (in days) between adjacent dates in a draw.
#' @param within An integer specifying the maximum time span (in days) of a draw.
#' @param detail Logical for whether return result per element of x.The default is FALSE, which returns one logical summarized by any().
#' @param align Character, define if the time span for each record should start ("left") or end ("right") at its current date. Defaults to "left".
#' @param dup.rm Logical for whether duplicated dates in x should be removed before calculation. Default is TRUE.
#' @param ... Additional argument passing to [data.table::as.IDate()] for date conversion.
#' @seealso [restrict_dates()]
#'
#' @return Single or a vector of logical for whether there is any draw from x satisfied the conditions
#' @export
#'
#' @examples
#' dates_of_records <- sample(seq(as.Date("2015-01-01"), as.Date("2021-12-31"), 7), 10)
#'
#' # whether there is any 3 records at least 30 days apart within 2 years
#' if_dates(dates_of_records, n = 3, apart = 30, within = 365 * 2)
#'
#' # specified either apart or within or both
#' if_dates(dates_of_records, n = 2, within = 365)
#'
if_dates <- function(x, n, apart = NULL, within = NULL, detail = FALSE, align = c("left", "right"), dup.rm = TRUE, ...) {
  if (all(is.null(apart), is.null(within))) stop("apart and within cannot both be NULL")

  # stopifnot("x must be character or Date" = any(is.character(x), lubridate::is.Date(x)))
  stopifnot(is.wholenumber(n))
  align <- rlang::arg_match0(align, c("left", "right"))

  # place holder for var names
  N <- len_enough <- period_id <- real_end <- start <- y <- NULL

  if (dup.rm) x <- unique(x)

  len <- length(x)

  if (len < n) {
    return(FALSE)
  }

  x <- data.table::as.IDate(x, ...)

  # just interpret apart if no within. see all_part.R for detail
  if (is.null(within)) {
    stopifnot(is.wholenumber(apart))
    return(all_apart(x, n, apart))
  }

  if (is.null(apart)) {
    # if just within, the adjacent n has the smallest gaps, if none of the rolling window < within, no other combinations will be so.
    stopifnot(is.wholenumber(within))

    x_roll <- data.table::frollapply(x = sort(x), n = n, align = align, FUN = function(x) sum(diff(x)) <= within)
    dtx <- data.table::data.table(x = as.numeric(x_roll == 1))

    # switch (align,
    #   left = data.table::setnafill(dtx, "locf"),
    #   right = data.table::setnafill(dtx, fill = 0)
    # )
    data.table::setnafill(dtx, fill = 0)

    if (detail) {
      return(as.logical(dtx[, x]))
    } else {
      return(any(as.logical(dtx[, x]), na.rm = TRUE))
    }
  } else {
    stopifnot(is.wholenumber(c(apart, within)))
    if (apart * (n - 1) >= within) stop("Condition is impossible as apart*(n - 1) cannot be greater than within")
    # overlap join dates and dates+within to get records falls in every within window starting at each date. This ensure the within condition, then calls all_apart to test the apart condition
    # tested against combn(sample, n, function(x) all(diff(sort(x)) >= m) & (diff(c(min(x), max(x))) <= within)) %>% any()
    dtx <- data.table::data.table(x = x, y = x, key = c("x", "y"))

    switch (align,
      left = {dty <- data.table::data.table(start = x, end = x + within, period_id = seq(1:len), key = c("start", "end"))},
      right = {dty <- data.table::data.table(start = x - within, end = x, period_id = seq(1:len), key = c("start", "end"))}
    )

    overlap <- data.table::foverlaps(dtx, dty)[, y := NULL]
    data.table::setorder(overlap, period_id, x)
    overlap[, N := .N, by = period_id]
    # filters with if (!start_valid) is used for reducing compute
    if (!detail) overlap <- overlap[N >= n]
    overlap[, real_end := data.table::last(x), by = period_id]
    if (!detail) overlap <- overlap[, len_enough := (real_end - start) >= apart * (n - 1)][len_enough == TRUE]
    if (nrow(overlap) == 0) {
      return(FALSE)
    }

    overlap <- overlap[, list(incl = all_apart(x, n, apart)), by = period_id]

    if (detail) {
      return(overlap[["incl"]])
    } else {
      return(any(overlap[["incl"]], na.rm = TRUE))
    }
  }
}
