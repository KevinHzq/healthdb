test_that("basic use works", {
  # generate test data
  n <- 10
  gap <- 30
  answer <- data.frame(clnt = 1:n, n_epi = sample(1:5, n, replace = TRUE))
  answer <- answer %>%
    dplyr::mutate(epi_start = sample(seq(as.Date("2018-01-01"), as.Date("2020-12-31"), by = 1), size = n, replace = TRUE),
           epi_end = epi_start + lubridate::days(sample(365:(3*365), size = n, replace = TRUE)))
  answer <- cut_period(answer, epi_start, epi_end, gap)
  # no. gaps = n_epi - 1
  answer_with_gap <- answer %>% dplyr::group_by(clnt) %>%
    dplyr::slice(setdiff(dplyr::row_number(), which(1:(dplyr::n()-1) %% 2 == 0) %>% sample(dplyr::first(n_epi) - 1)))
  # make some noise within epi that should be collapsed
  noise <- answer_with_gap %>% dplyr::slice_sample(prop = 0.2) %>%
    dplyr::mutate(noise_start = purrr::map2(segment_start, segment_end, ~ sample(seq(.x, .y, by = 1), sample(1:3, 1), replace = TRUE)),
                  noise_end = purrr::map2(noise_start, segment_end, ~ purrr::map_vec(.x, function(i) i + lubridate::days(sample(0:lubridate::time_length(.y - i, unit = "day"), 1))))) %>%
    tidyr::unnest(cols = c(noise_start, noise_end)) %>%
    dplyr::select(-c(segment_start, segment_end)) %>%
    dplyr::rename(segment_start = noise_start,
           segment_end = noise_end)
  df <- dplyr::bind_rows(answer_with_gap, noise) %>% dplyr::arrange(clnt, segment_start)

  out_df_start_only <- collapse_episode(df, clnt_id = clnt, start_dt = segment_start, gap = 30)
  out_df_with_end <- collapse_episode(df, clnt_id = clnt, start_dt = "segment_start", end_dt = segment_end, gap = 30)
  # test if episode grouping is correct
  expect_equal(out_df_start_only %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(epi_no)),
               answer %>% dplyr::group_by(clnt) %>% dplyr::summarise(n_epi = max(n_epi)))
  # test the summary of start/end is correct
  epi_sum_with_end <- out_df_with_end %>% dplyr::group_by(epi_id) %>% dplyr::summarise(dplyr::across(c(segment_start, epi_start_dt), min),
                                                                                           dplyr::across(c(segment_end, epi_stop_dt), max))
  expect_equal(epi_sum_with_end$epi_start_dt, epi_sum_with_end$segment_start)
  expect_equal(epi_sum_with_end$epi_stop_dt, epi_sum_with_end$segment_end)
  # test if overwrite works
  out_df_overwrite <- collapse_episode(df, clnt_id = "clnt", start_dt = segment_start, gap = 30, overwrite = clnt)
  expect_equal(max(out_df_overwrite$epi_no), 1)
})
