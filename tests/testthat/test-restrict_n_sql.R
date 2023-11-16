test_that("basic use works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  db <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n), type = "database")
  output_id <- restrict_n(db, clnt_id = clnt_id, n_per_clnt = keep_n, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
})

test_that("nothing change if n = 1", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  db <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n), type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(clnt_id) %>% dplyr::mutate(flag_restrict_n = 1)
  out_df <- restrict_n(db, clnt_id = clnt_id, n_per_clnt = 1, mode = "filter") %>% dplyr::collect()
  expect_equal(out_df, df)
  # also test if the output is grouped
  expect_false(dplyr::is_grouped_df(out_df))
})

test_that("count_by works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  db <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n), type = "database")
  output_id <- restrict_n(db, clnt_id = clnt_id, n_per_clnt = keep_n, count_by = dates, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
})

test_that("edge case - variable in external vector works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  db <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n), type = "database")
  clnt <- "clnt_id"
  grp <- "dates"
  output_id <- restrict_n(db, clnt_id = !!clnt, n_per_clnt = keep_n, count_by = !!grp, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
})

test_that("mode works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  db <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n), type = "database")
  output_id <- restrict_n(db, clnt_id = clnt_id, n_per_clnt = keep_n)
  expect_in("flag_restrict_n", names(output_id))
  output_id <- output_id %>% dplyr::filter(flag_restrict_n > 0) %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
})
