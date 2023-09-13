test_that("output class works", {
  nrows <- 25
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  df <- make_test_dat(nrows = nrows, n_any = n_any, n_all = n_all)
  db <- make_test_dat(nrows = nrows, n_any = n_any, n_all = n_all, type = "database")
  expect_s3_class(df, "data.frame")
  expect_s3_class(db, "tbl_sql")
  # make sure there is really data in tbl
  expect_equal(dplyr::collect(db) %>% nrow(), nrows)
})

test_that("n_any is correct", {
  nrows <- 25
  n_any <- nrows %/% 2
  n_all <- n_any %/% 3
  test_dat <- make_test_dat(vals_kept = letters, nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans")
  any <- test_dat %>% dplyr::filter(dplyr::if_any(dplyr::starts_with("diagx"), ~ . %in% letters))
  expect_equal(nrow(any), n_any)
  # also test with the answer_id column
  expect_equal(subset(test_dat, ans == "any") %>% nrow(), n_any - n_all)
  # no need to test n_all or noise as they are a function of n_any
})

test_that("edge case n_any = nrows works", {
  nrows <- 24
  n_any <- nrows
  n_all <- n_any %/% 3
  test_dat <- make_test_dat(vals_kept = letters, nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans")
  expect_equal(subset(test_dat, ans == "noise") %>% nrow(), 0)
})

test_that("edge case n_any = n_all = nrows works", {
  nrows <- 24
  n_any <- nrows
  n_all <- n_any
  test_dat <- make_test_dat(vals_kept = letters, nrows = nrows, n_any = n_any, n_all = n_all, answer_id = "ans")
  expect_equal(subset(test_dat, ans == "noise") %>% nrow(), 0)
  expect_equal(subset(test_dat, ans == "all") %>% nrow(), nrows)
})
