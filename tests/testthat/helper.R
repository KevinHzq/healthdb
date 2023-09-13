letters_n <- function(nrows = 25, type = "data.frame") {
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  make_test_dat(vals_kept = letters, nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans", type = type)
}

xnum_n <- function(x, nrows = 25, type = "data.frame") {
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  make_test_dat(vals_kept = paste0(x, 1:9), nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans", type = type)
}

btw_n <- function(date_range, n_ans = 5, type = "data.frame") {
  keep <- make_test_dat(vals_kept = letters, nrows = n_ans, n_any = n_ans, n_all = n_ans, answer_id = "ans", date_range = date_range)
  out <- letters_n()
  all <- dplyr::bind_rows(keep, out)

  if (type == "database") {
    con <- dbplyr::src_memdb()
    dplyr::copy_to(con, all, "db", temporary = TRUE, overwrite = TRUE)
    all <- dplyr::tbl(con, "db")
  }
  return(all)
}
