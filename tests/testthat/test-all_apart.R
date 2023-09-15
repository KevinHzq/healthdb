test_that("basic case works", {
  n <- 3
  apart <- 180
  set.seed(2023)
  dates <- sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = 1), 10)
  ans <- combn(dates, n, function(x) all(diff(sort(x)) >= apart)) %>% any()
  expect_equal(all_apart(dates, n, apart), ans)
})

test_that("special case n = 2 works", {
  n <- 2
  apart <- 180
  set.seed(2023)
  dates <- sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = 1), 10)
  ans <- combn(dates, n, function(x) all(diff(sort(x)) >= apart)) %>% any()
  expect_equal(all_apart(dates, n, apart), ans)
})

test_that("n = even number > 2 works", {
  n <- 6
  apart <- 30
  set.seed(2023)
  dates <- sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = 1), 10)
  ans <- combn(dates, n, function(x) all(diff(sort(x)) >= apart)) %>% any()
  expect_equal(all_apart(dates, n, apart), ans)
})
