test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  df <- make_test_dat()
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, mode = "filter")
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
  # also test mode
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within) %>% dplyr::filter(flag_restrict_date == 1)
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

test_that("align works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  within <- 365
  apart <- 7
  n <- 2
  df <- data.frame(clnt_id = 1, dates = x)
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, align = "left", mode = "flag")
  expect_equal(output_df$flag_restrict_date, as.numeric(ans))
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, align = "right", mode = "filter")
  ans_right <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(output_df$dates, x)
  expect_setequal(output_df$dates[output_df$flag_restrict_date == 1], x[ans_right])
})
