test_that("basic use works", {
  n <- sample(2:5, 1)
  apart <- sample(7:14, 1)
  within <- sample(30:365, 1)
  df <- make_test_dat()
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, mode = "filter")
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
  # also test mode
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within) %>% dplyr::filter(flag_restrict_date == 1)
  expect_setequal(output_df$clnt_id, ans_id)
  expect_message(restrict_dates(df, clnt_id, dates, n, apart, within, mode = "filter", verbose = TRUE), "excluded")
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

test_that("flag_at works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  within <- 365
  apart <- 7
  n <- 2
  df <- data.frame(clnt_id = 1, dates = x)
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, flag_at = "left", mode = "flag")
  expect_equal(output_df$flag_restrict_date, as.numeric(ans))
  output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, flag_at = "right", mode = "filter")
  ans_right <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(output_df$dates, x)
  expect_setequal(output_df$dates[output_df$flag_restrict_date == 1], x[ans_right])
})

test_that("check missing dates works", {
  n <- sample(2:5, 1)
  within <- sample(30:365, 1)
  df <- make_test_dat()
  df$dates[sample(1:nrow(df), 5)] <- NA
  expect_warning(restrict_dates(df, clnt_id, dates, n, within = within, uid = uid), "Removed 5 records")
})

