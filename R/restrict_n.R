#' Title
#'
#' @param data
#' @param clnt_id
#' @param n_per_clnt
#' @param count_by
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
restrict_n <- function(data, clnt_id = NULL, n_per_clnt, count_by = NULL, verbose = TRUE) {
  stopifnot(is.numeric(n_per_clnt))
  UseMethod("restrict_n")
}



