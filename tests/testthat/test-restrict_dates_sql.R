test_that("basic use works", {
  apart <- 7
  within <- 180
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  # walk, not sample, over n: n = 2, even, and odd n hit distinct branches
  # in the generated SQL
  purrr::walk(2:5, function(n) {
    output_df <- restrict_dates(db, clnt_id, dates, n, apart, within, uid = uid, force_collect = TRUE, mode = "filter")
    ans_id <- test_apart_within(df, n, apart, within)
    expect_setequal(output_df$clnt_id, ans_id)
  })
  expect_message(restrict_dates(db, clnt_id, dates, 2, apart, within, uid = uid, verbose = TRUE), "flagged")
})

test_that("apart only works", {
  apart <- 7
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  # walk, not sample, over n: n = 2, even, and odd n hit distinct branches
  # in the generated SQL
  purrr::walk(2:5, function(n) {
    output_df <- restrict_dates(db, clnt_id, dates, n, apart, uid = uid, mode = "filter") %>%
      dplyr::collect()
    ans_id <- test_apart_within(df, n, apart)
    expect_setequal(output_df$clnt_id, ans_id)
  })
})

test_that("within only works", {
  skip_on_cran()
  within <- 180
  db <- make_test_dat(type = "database")
  df <- dplyr::collect(db)
  purrr::walk(2:5, function(n) {
    output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
    ans_id <- test_apart_within(df, n, within = within)
    expect_setequal(output_df$clnt_id, ans_id)
  })
  n <- 2
  ans_id <- test_apart_within(df, n, within = within)
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
  db <- memdb_tbl(df)
  output_df <- restrict_dates(db, clnt_id, dates, n, within = within, uid = uid, mode = "filter") %>% dplyr::collect()
  ans_id <- test_apart_within(df, n, within = within)
  expect_setequal(output_df$clnt_id, ans_id)
})

test_that("flag_at works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  within <- 365
  n <- 2
  db <- memdb_tbl(dplyr::tibble(clnt_id = 1, dates = x, uid = 1:length(x)))
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
  db <- memdb_tbl(df)
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
  db <- memdb_tbl(dplyr::tibble(clnt_id = 1, dates = x, uid = 1:length(x)))
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

test_that("edge case - apart boundary, same-date, and too-few-record clients", {
  # client 1: gap exactly equal to apart (>= boundary must flag)
  # client 2: duplicated single date (gap 0 must not flag)
  # client 3: fewer records than n (must not flag)
  db <- memdb_tbl(dplyr::tibble(
    clnt_id = c(1, 1, 2, 2, 3),
    dates = as.Date(c("2020-01-01", "2020-01-08", "2020-03-01", "2020-03-01", "2020-05-01")),
    uid = 1:5
  ))
  out <- restrict_dates(db, clnt_id, dates, n = 2, apart = 7, uid = uid, mode = "filter") %>% dplyr::collect()
  expect_setequal(out$clnt_id, 1)

  # n = 3: client 1 has both gaps exactly apart; client 2 has one gap one
  # day short
  db3 <- memdb_tbl(dplyr::tibble(
    clnt_id = c(1, 1, 1, 2, 2, 2),
    dates = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15", "2020-02-01", "2020-02-08", "2020-02-14")),
    uid = 1:6
  ))
  out3 <- restrict_dates(db3, clnt_id, dates, n = 3, apart = 7, uid = uid, mode = "filter") %>% dplyr::collect()
  expect_setequal(out3$clnt_id, 1)
})

test_that("edge case - NA dates without check_missing do not error or false-flag", {
  # client 1 meets the condition with valid dates plus an NA record;
  # client 2 has only NA dates and must never be flagged
  db <- memdb_tbl(dplyr::tibble(
    clnt_id = c(1, 1, 1, 2, 2),
    dates = as.Date(c("2020-01-01", "2020-01-08", NA, NA, NA)),
    uid = 1:5
  ))
  out_apart <- restrict_dates(db, clnt_id, dates, n = 2, apart = 7, uid = uid, mode = "filter") %>% dplyr::collect()
  expect_setequal(out_apart$clnt_id, 1)
  out_within <- restrict_dates(db, clnt_id, dates, n = 2, within = 30, uid = uid, mode = "filter") %>% dplyr::collect()
  expect_setequal(out_within$clnt_id, 1)
})
