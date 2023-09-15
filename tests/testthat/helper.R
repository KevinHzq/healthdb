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

iclnt_jdates <- function(i, j, dup, date_range = c(as.Date("2015-01-01"), as.Date("2021-01-31")), type = "data.frame") {
  dat <- purrr::map2(i, j, ~ tidyr::expand_grid(clnt_id = .x, dates = seq(date_range[1], date_range[2], length.out = .y)))
  dat <- append(dat, purrr::map2(i, dup, ~ dplyr::tibble(clnt_id = rep(.x, each = .y), dates = date_range[1])))

  test_dat <- purrr::list_rbind(dat)

  if (type == "database") {
    con <- dbplyr::src_memdb()
    dplyr::copy_to(con, test_dat, "db", temporary = TRUE, overwrite = TRUE)
    test_dat <- dplyr::tbl(con, "db")
  }

  return(test_dat)
}

# internal test function that should be ran on make_test_dat() output
test_apart_within <- function(data, n, apart = 0, within = Inf) {
  data <- data %>%
    dplyr::group_by(.data[["clnt_id"]]) %>%
    dplyr::filter(dplyr::n_distinct(.data[["dates"]]) >= n)

  keep <- data %>%
    dplyr::summarise(met = utils::combn(.data[["dates"]] %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any())

  keep <- keep %>%
    dplyr::filter(met) %>%
    dplyr::pull(.data[["clnt_id"]])

  return(keep)
}

test_if_dates <- function(x, n, apart = 0, within = Inf, dup.rm = TRUE) {
  if (dup.rm) utils::combn(x %>% unique(), n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any()
  else utils::combn(x, n, function(x) all(diff(sort(x)) >= apart) & (diff(c(min(x), max(x))) <= within)) %>% any()
}
