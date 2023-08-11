#' Report number of distinct value in a column across data frames
#'
#' @description
#' This function is intended to mimic dplyr::n_distinct() for multiple inputs. It is useful to report the number of clients through out a series of inclusion or exclusion. An use case could be getting the Ns for the sample definition flowchart in an epidemiological study.
#'
#' @param ... Data frames or remote tables (e.g., from dbplyr)
#' @param on The column to report on. It must be present in all data sources.
#'
#' @return A sequence of the number of distinct `on` for each data frames
#' @export
#'
#' @examples
#' # some exclusions
#' iris_1 <- subset(iris, Petal.Length > 1)
#' iris_2 <- subset(iris, Petal.Length > 2)
#'
#' # get n at each operation
#' n <- report_n(iris, iris_1, iris_2, on = Species)
#' n
#'
#' # get the difference at each step
#' diff(n)
report_n <- function(..., on) {
  rlang::check_required(on)

  on <- rlang::as_name(rlang::enquo(on))

  col_nm <- sapply(list(...), function(x) {if ("dtplyr_step" %in% class(x)) x[["vars"]] else names(x)})

  if (all(sapply(col_nm, function(x) on %in% x))) stop("All data must have the ", on, " column.")

  sapply(list(...), function(x) dplyr::group_by(x, .data[[on]]) %>% dplyr::n_groups())
}
