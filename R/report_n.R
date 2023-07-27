#' Report number of distinct value in a column across data frames
#'
#' @description
#' This function is intended to mimic dplyr::n_distinct() for multiple inputs. It is useful to report the number of clients through out a series of inclusion or exclusion. An use case could be getting the Ns for the sample definition flowchart in an epidemiological study.
#'
#' @param ... Data frames or remote tables (e.g., from dbplyr)
#' @param on_nm The column name to report on. It must be present in all data sources.
#'
#' @return A sequence of distinct n in on_nm for each data frames
#' @export
#'
#' @examples
#' #some exclusions
#' iris_1 <- subset(iris, Petal.Length > 1)
#' iris_2 <- subset(iris, Petal.Length > 2)
#'
#' #get n at each operation
#' n <- report_n(iris, iris_1, iris_2, on_nm = "Species")
#' n
#'
#' #get the difference at each step
#' diff(n)
report_n <- function(..., on_nm) {
  if (!is.character(on_nm)) stop("The `on_nm` argument must be character.")

  if (all(sapply(list(...), function(x) on_nm %in% names(x)))) {
    sapply(list(...), function(x) dplyr::group_by(x, .data[[on_nm]]) %>% dplyr::n_groups())
  }

  else stop("All data must have the", on_nm, "column.")
}


