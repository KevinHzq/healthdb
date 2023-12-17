test_that("basic use works", {
  n <- 3
  apart <- 30
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid, force_collect = TRUE, mode = "filter")
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("within only works", {
  n <- 2
  within <- 365
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
  #also test uid error
  expect_error(restrict_dates(db, clnt_id, dates, n, within = within, mode = "filter"), "uid")
  # also test mode
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid) %>% dplyr::collect() %>%
    dplyr::filter(flag_restrict_dates > 0)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("do not count same date works for database", {
  n <- 3
  within <- 365
  df <- data.frame(uid = n*2+1*3, clnt_id = rep(1:3, each = n*2+1), dates = rep(c(rep(as.Date("2020-01-01"),n*2-1), as.Date("2020-01-01")+within+1, as.Date("2020-01-01")+within*2+1), 3))
  db <- dbplyr::memdb_frame(df)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("align works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  within <- 365
  n <- 2
  db <- dbplyr::memdb_frame(clnt_id = 1, dates = x, uid = 1:length(x))
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, align = "left") %>% dplyr::collect()
  expect_setequal(output_df$flag_restrict_dates, as.numeric(ans))
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, align = "right", mode = "filter") %>% dplyr::collect()
  ans_right <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  expect_setequal(output_df$dates[output_df$flag_restrict_dates == 1] %>% as.Date(origin = "1970-01-01"), x[ans_right])
})
