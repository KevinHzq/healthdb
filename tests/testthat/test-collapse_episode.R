test_that("basic use works", {
  # generate test data
  data <- episode_df(10, 30)
  answer <- data[[1]]
  df <- data[[2]]

  out_df_start_only <- collapse_episode(df, clnt_id = clnt, start_dt = segment_start, gap = 30)
  out_df_with_end <- collapse_episode(df, clnt_id = clnt, start_dt = "segment_start", end_dt = segment_end, gap = 30)
  # test if episode grouping is correct
  expect_equal(
    out_df_start_only %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(epi_no)),
    answer %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(n_epi))
  )
  # test the summary of start/end is correct
  epi_sum_with_end <- out_df_with_end %>%
    dplyr::group_by(epi_id) %>%
    dplyr::summarise(
      dplyr::across(c(segment_start, epi_start_dt), min),
      dplyr::across(c(segment_end, epi_stop_dt), max)
    )
  expect_equal(epi_sum_with_end$epi_start_dt, epi_sum_with_end$segment_start)
  expect_equal(epi_sum_with_end$epi_stop_dt, epi_sum_with_end$segment_end)
  # test if overwrite works
  out_df_overwrite <- collapse_episode(df, clnt_id = "clnt", start_dt = segment_start, gap = 30, overwrite = clnt, .dt_trans = lubridate::ymd)
  expect_equal(max(out_df_overwrite$epi_no), 1)
})

test_that("database input works", {
  # generate test data
  data <- episode_df(10, 30, "db")
  answer <- data[[1]]
  db <- data[[2]]

  out_df_start_only <- collapse_episode(db, clnt_id = clnt, start_dt = segment_start, gap = 30) %>%
    dplyr::collect()
  out_df_with_end <- collapse_episode(db, clnt_id = clnt, start_dt = "segment_start", end_dt = segment_end, gap = 30) %>%
    dplyr::collect()
  # test if episode grouping is correct
  expect_equal(
    out_df_start_only %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(epi_no)),
    answer %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(n_epi))
  )
  # test the summary of start/end is correct
  epi_sum_with_end <- out_df_with_end %>%
    dplyr::group_by(epi_id) %>%
    dplyr::summarise(
      dplyr::across(c(segment_start, epi_start_dt), min),
      dplyr::across(c(segment_end, epi_stop_dt), max)
    )
  expect_equal(epi_sum_with_end$epi_start_dt, epi_sum_with_end$segment_start)
  expect_equal(epi_sum_with_end$epi_stop_dt, epi_sum_with_end$segment_end)
  # test if overwrite works
  out_df_overwrite <- collapse_episode(db, clnt_id = "clnt", start_dt = segment_start, gap = 30, overwrite = clnt) %>% dplyr::collect()
  expect_equal(max(out_df_overwrite$epi_no), 1)
  # test name conflict warning
  db <- dplyr::rename(db, epi_id = segment_id)
  expect_error(collapse_episode(db, clnt_id = clnt, start_dt = segment_start, gap = 30), "conflict")
})
