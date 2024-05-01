test_that("basic use works", {
  df <- letters_n()
  ans_rows <- sample(1:5, size = nrow(df), replace = TRUE)
  unt <- "week"
  df <- df %>%
    dplyr::mutate(
      add = ans_rows,
      end_date = purrr::map2_vec(dates, add, ~ .x %m+% lubridate::period(.y, units = unt) %m-% lubridate::days(1)))
  out_df <- df %>% cut_period(start = dates, end = end_date, len = 1, unit = unt)
  expect_equal(nrow(out_df), sum(ans_rows))
  expect_in(names(df), names(out_df))
})

test_that("date transform works", {
  df <- letters_n()
  ans_rows <- sample(1:5, size = nrow(df), replace = TRUE)
  unt <- "month"
  df <- df %>%
    dplyr::mutate(
      add = ans_rows,
      end_date = purrr::map2_vec(dates, add, ~ .x %m+% lubridate::period(.y, units = unt) %m-% lubridate::days(1)),
      dplyr::across(dplyr::where(lubridate::is.Date), as.character))
  expect_error(cut_period(df, start = dates, end = end_date, len = 1, unit = unt), "is not Date")
  out_df <- df %>% cut_period(start = dates, end = end_date, len = 1, unit = unt, .dt_trans = as.Date)
  expect_equal(nrow(out_df), sum(ans_rows))
})

test_that("edge case start > end", {
  df <- letters_n()
  unt <- "month"
  df <- df %>%
    dplyr::mutate(
      end_date = dates - 7)
  expect_error(df %>% cut_period(start = dates, end = end_date, len = 1, unit = unt), ">")
})

test_that("edge case missing start", {
  df <- letters_n()
  unt <- "month"
  df <- df %>%
    dplyr::mutate(
      end_date = ifelse(uid != 1, dates + 7, NA))
  expect_error(df %>% cut_period(start = dates, end = end_date, len = 1, unit = unt), "missing")
})
