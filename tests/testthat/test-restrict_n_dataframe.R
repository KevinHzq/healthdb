test_that("basic use works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n))
  output_id <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n) %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
  # also test the n in output
  expect_output(restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, verbose = TRUE), glue::glue("{length(c(keep_id, out_id))} clients in the input, {length(out_id)}"))
})

test_that("nothing change if n = 1", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n))
  out_df <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = 1) %>% dplyr::as_tibble()
  expect_equal(out_df, df %>% dplyr::arrange(clnt_id))
  # also test if the output is grouped
  expect_false(dplyr::is_grouped_df(out_df))
})

test_that("count_by works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n))
  out_id <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, count_by = dates) %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, keep_id)
})

test_that("edge case - variable in external vector works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n))
  clnt <- "clnt_id"
  grp <- "dates"
  out_id <- restrict_n(df, clnt_id = !!clnt, n_per_clnt = keep_n, count_by = !!grp) %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, keep_id)
})

