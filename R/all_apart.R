all_apart <- function(x, n, apart) {
  # helper function called inside if_dates
  # return logical if there could be at least one set of n elements taken from x that adjacent elements in the set are at least certain days apart
  # searching by sliding apart windows from both ends toward the middle
  # there must be n - i*2 records in between the windows
  # tested against combn(x, n, function(x) all(diff(sort(x)) >= apart)) %>% any()
  dtx <- data.table::data.table(x = x)
  incl <- c(NULL)
  final_gap <- NULL
  for (i in 1:(n %/% 2)) {
    if ((n - i * 2) == 0) {
      if (n == 2) {
        final_gap <- x
      } else {
        final_gap <- dtx[dtx[[paste("in", i - 1, "apart", sep = "_")]] == TRUE, x]
        if (length(final_gap) == 0) {
          return(FALSE)
        }
      }
      final_gap <- (max(final_gap, na.rm = TRUE) - min(final_gap, na.rm = TRUE)) >= apart
      return(final_gap)
    } else {
      if (i == 1) {
        dtx[, c(paste("in", i, "apart", sep = "_")) := data.table::between(x, min(x, na.rm = TRUE) + apart, max(x, na.rm = TRUE) - apart)]
      } else {
        irange <- dtx[dtx[[paste("in", i - 1, "apart", sep = "_")]] == TRUE, x]
        dtx[, c(paste("in", i, "apart", sep = "_")) := data.table::between(x, min(irange, na.rm = TRUE) + apart, max(irange, na.rm = TRUE) - apart)]
      }
      incl[i] <- dtx[, sum(.SD) >= n - i * 2, .SDcols = paste("in", i, "apart", sep = "_")]
      if (incl[i] == FALSE) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
