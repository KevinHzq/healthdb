test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  df <- make_test_dat()
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, mode = "filter")
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
  # also test mode
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within) %>% dplyr::filter(flag_restrict_dates == 1)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("edge case - var in external vector works", {
  n <- 3
  apart <- 30
  within <- 365
  clnt <- "clnt_id"
  dates <- "dates"
  df <- make_test_dat()
  output_df <- restrict_dates(df, !!clnt, !!dates, n, apart, within, mode = "filter")
  expect_s3_class(output_df, "data.frame")
})

test_that("start_valid works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  within <- 365
  apart <- 7
  n <- 2
  df <- data.frame(clnt_id = 1, dates = x)
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, start_valid = TRUE, mode = "filter")
  ans_dates <- x[cummax(ans) > 0]
  expect_setequal(output_df$dates, ans_dates)
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, start_valid = FALSE, mode = "filter")
  expect_setequal(output_df$dates, x)
  # also test mode
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, start_valid = FALSE, mode = "flag") %>% dplyr::filter(flag_restrict_dates == 1)
  expect_setequal(output_df$dates, x)
})
