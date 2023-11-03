test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid, force_collect = TRUE)
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("within only works", {
  n <- 2
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid) %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
  #also test uid error
  expect_error(restrict_dates(db, clnt_id, dates, n, within = within), "uid")
})

test_that("do not count same date works for database", {
  n <- 3
  within <- 365
  df <- data.frame(uid = n*2+1*3, clnt_id = rep(1:3, each = n*2+1), dates = rep(c(rep(as.Date("2020-01-01"),n*2-1), as.Date("2020-01-01")+within+1, as.Date("2020-01-01")+within*2+1), 3))
  db <- dbplyr::memdb_frame(df)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid) %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
})
