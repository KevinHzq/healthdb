#' Find value corresponding to input vector using a look-up table
#'
#' @param x A variable name in a data.frame; this function should be called inside dplyr::mutate().
#' @param link A formula in the form: name_of_x_in_lu ~ name_of_target_value. The left-hand-side can be omitted if x's name is also x in the look-up.
#' @param lu Look-up table in data.frame class.
#' @param verbose A logical for whether warn for missing values in the output.
#'
#' @return A vector of matched values.
#' @export
#'
#' @examples
#'
#' df <- data.frame(drug_code = 1:10)
#' lu <- data.frame(drug_id = 1:20, drug_code = as.character(1:10), drug_name = sample(letters, 20))
#'
#' df %>% dplyr::mutate(
#'   drug_nm = lookup(drug_code, drug_id ~ drug_name, lu),
#'   # this will work as lu also has drug_code column
#'   drug_nm = lookup(drug_code, ~ drug_name, lu)
#' )
lookup <- function(x, link, lu, verbose = getOption("healthdb.verbose")) {
  # input checks
  stopifnot(rlang::is_formula(link),
            is.data.frame(lu))

  # capture variable name of x
  x_nm <- rlang::as_name(rlang::enquo(x))

  # capture formula components
  lhs <- rlang::f_lhs(link)
  rhs <- rlang::f_text(link)

  if (is.null(lhs)) lhs <- x_nm
  else lhs <- rlang::as_name(lhs)

  if (is.character(x)) {
    index <- data.table::chmatch(x, as.character(lu[[lhs]]))
  }
  else index <- match(x, lu[[lhs]])

  y <- lu[[rhs]][index]

  if (verbose) {
    if(any(is.na(y))) warning("Matched output contains missing value(s)")
  }

  y
}
