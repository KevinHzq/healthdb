test_that("basic case works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, 30, 365)
  expect_equal(if_dates(dates, 3, 30, 365), ans)
})

test_that("dup.rm works", {
  dates <- c(rep(as.Date("2023-12-01"), 3), as.Date("2023-12-08"), as.Date("2023-12-16"))
  expect_true(if_dates(dates, 5, 0, 30, dup.rm = FALSE))
  expect_false(if_dates(dates, 5, 0, 30, dup.rm = TRUE))
})

test_that("input checks work", {
  dates <- c(as.Date("2023-12-08"), as.Date("2023-12-16"))
  expect_error(if_dates(dates, n = 3), "both be NULL")
  expect_error(if_dates(dates, n = 2, apart = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 7, within = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 1.5, within = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 7, within = 5), "impossible")
  # must be false if dates is shorter than n
  expect_false(if_dates(dates, n = 5, apart = 3))
})

test_that("apart only works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, apart = 30)
  expect_equal(if_dates(dates, 3, apart = 30), ans)
})

test_that("within only works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, within = 365)
  expect_equal(if_dates(dates, 3, within = 365), ans)
})

test_that("straight false due to no gap < within works", {
  dates <- c(as.Date("2023-01-01"), as.Date("2023-03-01"), as.Date("2023-05-01"))
  expect_false(if_dates(dates, 2, apart = 7, within = 30))
})

test_that("detail works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  w <- 365
  out <- if_dates(x, n = 2, within = 365, detail = TRUE)
  expect_equal(out, ans)
})
