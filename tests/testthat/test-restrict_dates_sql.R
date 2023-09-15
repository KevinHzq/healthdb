test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, apart, within, force_collect = TRUE)
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("within only works", {
  n <- 2
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within) %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
})


