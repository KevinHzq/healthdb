test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  df <- make_test_dat()
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within)
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("edge case - var in external vector works", {
  n <- 3
  apart <- 30
  within <- 365
  clnt <- "clnt_id"
  dates <- "dates"
  df <- make_test_dat()
  output_df <- restrict_dates(df, !!clnt, !!dates, n, apart, within)
  expect_s3_class(output_df, "data.frame")
})

