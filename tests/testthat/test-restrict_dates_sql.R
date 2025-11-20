test_that("basic use works", {
  n <- sample(2:5, 1)
  apart <- sample(7:14, 1)
  within <- sample(30:365, 1)
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid, force_collect = TRUE, mode = "filter")
  ans_id <- test_apart_within(df, n, apart, within)
  expect_setequal(output_df$clnt_id, ans_id)
  expect_message(restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid, verbose = TRUE), "flagged")
})

test_that("apart only works", {
  n <- sample(2:5, 1)
  apart <- sample(7:14, 1)
  within <- sample(30:365, 1)
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, apart, uid = uid, mode = "filter") %>%
    dplyr::collect()
  ans_id <- test_apart_within(df, n, apart)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("within only works", {
  skip_on_cran()
  n <- sample(2:5, 1)
  within <- sample(30:365, 1)
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
  # also test uid error
  expect_error(restrict_dates(db, clnt_id, dates, n, within = within, mode = "filter"), "uid")
  # also test mode
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid) %>%
    dplyr::collect() %>%
    dplyr::filter(flag_restrict_date > 0)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("do not count same date works for database", {
  n <- 3
  within <- 365
  df <- data.frame(uid = n * 2 + 1 * 3, clnt_id = rep(1:3, each = n * 2 + 1), dates = rep(c(rep(as.Date("2020-01-01"), n * 2 - 1), as.Date("2020-01-01") + within + 1, as.Date("2020-01-01") + within * 2 + 1), 3))
  db <- dbplyr::memdb_frame(df)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("flag_at works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  within <- 365
  n <- 2
  db <- dbplyr::memdb_frame(clnt_id = 1, dates = x, uid = 1:length(x))
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, flag_at = "left") %>% dplyr::collect()
  expect_setequal(output_df$flag_restrict_date, as.numeric(ans))
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, flag_at = "right", mode = "filter") %>% dplyr::collect()
  ans_right <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  expect_setequal(output_df$dates[output_df$flag_restrict_date == 1] %>% as.Date(origin = "1970-01-01"), x[ans_right])
})

test_that("count same date works for database", {
  n <- 3
  within <- 365
  df <- data.frame(uid = n * 2 + 1 * 3, clnt_id = rep(1:3, each = n * 2 + 1), dates = rep(c(rep(as.Date("2020-01-01"), n * 2 - 1), as.Date("2020-01-01") + within + 1, as.Date("2020-01-01") + within * 2 + 1), 3))
  db <- dbplyr::memdb_frame(df)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter", dup.rm = FALSE) %>% dplyr::collect()
  ans <- df %>%
    dplyr::mutate(ans = if_date(dates, n = n, within = within, dup.rm = FALSE)) %>%
    dplyr::filter(ans)
  expect_setequal(output_df$clnt_id, ans$clnt_id)
})

test_that("check missing dates works", {
  n <- sample(2:5, 1)
  within <- sample(30:365, 1)
  df <- make_test_dat()
  df$dates[sample(1:nrow(df), 5)] <- NA
  db <- memdb_tbl(df)
  expect_warning(restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, check_missing = TRUE), "Removed 5 records")
})

test_that("edge case duplicated dates and n > 2", {
  x <- as.Date(c("2010-01-01", "2010-01-01", "2010-03-01", "2010-03-01", "2010-04-08"))
  within <- 365
  n <- 3
  ans <- if_date(x, n, within = within, detail = TRUE)
  db <- dbplyr::memdb_frame(clnt_id = 1, dates = x, uid = 1:length(x))
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, flag_at = "right") %>%
    # test compatibility with compute()
    dplyr::compute() %>%
    dplyr::collect()
  expect_setequal(output_df$flag_restrict_date, as.numeric(ans))
})

test_that("check apart and within works for even and odd ns", {
  skip_on_cran()
  purrr::walk(2:6, function(n) {
    apart <- sample(7:14, 1)
    within <- sample(30:365, 1) + apart
    df <- make_test_dat()
    db <- memdb_tbl(df)
    output_df <- restrict_dates(df, clnt_id, dates, n, apart, within, uid = uid)
    output_db <- restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid) %>%
      dplyr::collect() %>%
      dplyr::arrange(clnt_id, dates)
    expect_equal(output_db$flag_restrict_date, output_df$flag_restrict_date)
    ans_id <- test_apart_within(df, n, apart, within)
    expect_setequal(subset(output_db, flag_restrict_date == 1)$clnt_id, ans_id)
    expect_setequal(subset(output_df, flag_restrict_date == 1)$clnt_id, ans_id)
  })
})
