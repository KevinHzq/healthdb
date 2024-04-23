#' Make test data
#'
#' @description
#' Make a toy data set for testing and demo. This is for internal use purpose and not intended to be called by users.
#'
#'
#' @param vals_kept A vector of values that suppose to be identified.
#' @param noise_val A vector of values that are not meant to be identified.
#' @param IDs A vector of client IDs.
#' @param date_range A vector of all possible dates in the data.
#' @param nrows Number of rows of the output.
#' @param n_any Number of rows to be identified if the criteria is that if any target column contains certain values.
#' @param n_all Number of rows to be identified if the criteria is that if all target columns contain certain values.
#' @param seed Seed for random number generation.
#' @param answer_id Column name for the indicator of how the row should be identified: any, all, and noise.
#' @param type Output type, "data.frame" or "database".
#'
#' @return A data.frame or remote table from 'dbplyr'.
#' @export
#'
#' @examples
#' make_test_dat()
make_test_dat <- function(vals_kept = c("304", "305", 3040:3049, 3050:3059), noise_val = "999", IDs = 1:50, date_range = seq(as.Date("2015-01-01"), as.Date("2020-12-31"), by = 1), nrows = 100, n_any = 50, n_all = 10, seed = 2023, answer_id = NULL, type = c("data.frame", "database")) {
  stopifnot(
    n_all <= n_any,
    nrows >= n_any
  )

  type <- rlang::arg_match(type)

  set.seed(seed)

  n_noise <- nrows - n_any

  # contruct the answer rows and noise separately then combine

  df_any <- data.frame(
    diagx = sample(vals_kept, size = n_any - n_all, replace = TRUE),
    diagx_1 = sample(c(NA, vals_kept, noise_val), size = n_any - n_all, replace = TRUE),
    diagx_2 = sample(c(NA, noise_val), size = n_any - n_all, replace = TRUE)
  )

  df_all <- data.frame(
    diagx = sample(vals_kept, size = n_all, replace = TRUE),
    diagx_1 = sample(vals_kept, size = n_all, replace = TRUE),
    diagx_2 = sample(vals_kept, size = n_all, replace = TRUE)
  )

  df_noise <- data.frame(
    diagx = sample(c(noise_val), size = n_noise, replace = TRUE),
    diagx_1 = sample(c(noise_val, NA), size = n_noise, replace = TRUE),
    diagx_2 = sample(c(noise_val, NA), size = n_noise, replace = TRUE)
  )

  test_dat <- dplyr::bind_rows(any = df_any, all = df_all, noise = df_noise, .id = answer_id) %>%
    dplyr::mutate(
      uid = dplyr::row_number(),
      clnt_id = sample(IDs, size = nrows, replace = TRUE),
      dates = sample(date_range, size = nrows, replace = TRUE), .before = 0
    ) %>%
    dplyr::arrange(.data[["clnt_id"]], .data[["dates"]])

  if (type == "database") {
    # these cannot produce unique frame if result was called after another run of the function
    # con <- dbplyr::src_memdb()
    # dplyr::copy_to(con, test_dat, "db", temporary = TRUE, overwrite = TRUE)
    # test_dat <- dplyr::tbl(con, "db")

    # con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    # test_dat <- dbplyr::copy_inline(con, test_dat) %>%
    #   dplyr::mutate(dates = julianday(dates))

    # test_dat <- dbplyr::tbl_memdb(test_dat, name = stringi::stri_rand_strings(1, 20))

    test_dat <- memdb_tbl(test_dat)
  }

  return(test_dat)
}
