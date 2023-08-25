#' Report number of distinct value in a column across data frames
#'
#' @description
#' This function is intended to mimic dplyr::n_distinct() for multiple inputs. It is useful to report the number of clients through out a series of inclusion or exclusion. An use case could be getting the Ns for the sample definition flowchart in an epidemiological study.
#'
#' @param ... Data frames or remote tables (e.g., from dbplyr)
#' @param on The column to report on. It must be present in all data sources.
#' @param force_proceed A logical for whether to ask for user input in order to proceed when the data is not local data.frames, and a query needs to be executed before reporting. The default is TRUE to let user be aware of that the query execution may be slow. Use options(odcfun.force_proceed = FALSE) to suppress the prompt once and for all.
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
report_n <- function(..., on, force_proceed = getOption("odcfun.force_proceed")) {
  rlang::check_required(on)

  on <- rlang::as_name(rlang::enquo(on))

  col_nm <- sapply(list(...), function(x) {if ("dtplyr_step" %in% class(x)) x[["vars"]] else names(x)})

  if (all(sapply(col_nm, function(x) on %in% x))) stop("All data must have the ", on, " column.")

  sapply(list(...), function(x) {
    #ask for user input if data is remote
    if (!force_proceed & !is.data.frame(x)) {
      proceed <- readline(prompt = "\nThe data is not a data.frame. The query has to be executed (may be slow) in order to be summarized. Proceed? [y/n]")

      if (proceed == "n") stop("Try collect() the data first, or force_proceed = TRUE to slience the prompt.")
    }

    dplyr::group_by(x, .data[[on]]) %>% dplyr::n_groups()
  })
}
