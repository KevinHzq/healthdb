test_that("basic use works", {
  n <- 10
  unt <- "year"
  start <- sample(seq(as.Date("1970-01-01"), as.Date("2023-12-31"), by = 1), size = n)
  ans <- rnorm(n, mean = 30, sd = 10)
  end <- start + lubridate::duration(ans, unt)
  out <- compute_duration(start, end, unit = unt) %>% round(2)
  expect_equal(out, round(ans, 2))
  # single end date
  ans_single <- purrr::map_dbl(start, ~ (as.Date("2020-10-27") - .) / 365.25)
  out <- compute_duration(start, as.Date("2020-10-27"), unit = unt) %>% round(2)
  expect_equal(out, round(ans_single, 2))
  # also test grouped output
  out_fct <- compute_duration(start, end, lower_brks = c(0, 19, 25, 35, 45, 55))
  expect_equal(levels(out_fct), c("<19", "19-24", "25-34", "35-44", "45-54", "55+"))
})

test_that("date transform works", {
  unt <- "month"
  start <- paste("2023", c(1, 3, 5), "01", sep = "-")
  end <- paste("2023", c(2, 4, 6), "01", sep = "-")
  out <- compute_duration(start, end, unit = unt) %>% round(1)
  # implicit trans chr to date
  expect_equal(out, rep(1, 3))
  # trans with default ymd
  out <- compute_duration(start, end, unit = unt, trans = TRUE) %>% round(1)
  expect_equal(out, rep(1, 3))
  # trans with supplied ydm
  out <- compute_duration(start, end, unit = unt, trans = TRUE, .transfn = lubridate::ydm) %>% round(1)
  expect_equal(out, rep(0, 3))
})
