test_that("basic use works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n))
  output_id <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
  # also test the n in output
  expect_output(restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, verbose = TRUE, mode = "filter"), glue::glue("{length(c(keep_id, out_id))} clients in the input, {length(out_id)}"))
})

test_that("nothing change if n = 1", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n))
  out_df <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = 1, mode = "filter") %>% dplyr::as_tibble()
  expect_equal(out_df, df %>% dplyr::arrange(clnt_id) %>% dplyr::mutate(flag_restrict_n = 1))
  # also test if the output is grouped
  expect_false(dplyr::is_grouped_df(out_df))
})

test_that("count_by works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n))
  out_id <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, count_by = dates, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, keep_id)
  # also test flag  mode
  out_flag <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n, count_by = dates) %>% dplyr::filter(flag_restrict_n> 0) %>% dplyr::pull(clnt_id)
  expect_equal(out_flag, out_id)
})

test_that("edge case - variable in external vector works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n), dup = c(0, dup_n))
  clnt <- "clnt_id"
  grp <- "dates"
  out_id <- restrict_n(df, clnt_id = !!clnt, n_per_clnt = keep_n, count_by = !!grp, mode = "filter") %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, keep_id)
})

test_that("mode works", {
  keep_n <- 3
  dup_n <- 1
  keep_id <- 1:3
  out_id <- 5:6
  df <- iclnt_jdates(i = list(keep_id, out_id), j = c(keep_n, keep_n - dup_n - 1), dup = c(0, dup_n))
  output_id <- restrict_n(df, clnt_id = clnt_id, n_per_clnt = keep_n)
  expect_in("flag_restrict_n", names(output_id))
  output_id <- output_id %>% dplyr::filter(flag_restrict_n > 0) %>% dplyr::pull(clnt_id)
  expect_setequal(output_id, keep_id)
})
