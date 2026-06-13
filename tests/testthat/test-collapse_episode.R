test_that("basic use works", {
  # seed the random fixture so failures are reproducible
  set.seed(1)
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
  # start-only path: epi_stop_dt should collapse to the episode's max start
  epi_sum_start_only <- out_df_start_only %>%
    dplyr::group_by(epi_id) %>%
    dplyr::summarise(
      max_start = max(segment_start),
      epi_stop_dt = dplyr::first(epi_stop_dt)
    )
  expect_equal(epi_sum_start_only$epi_stop_dt, epi_sum_start_only$max_start)
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
  # seed the random fixture so failures are reproducible
  set.seed(2)
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

test_that("data.frame input errors on name conflicts", {
  df <- data.frame(
    clnt = 1L,
    segment_start = as.Date(c("2020-01-01", "2020-02-01")),
    epi_id = c("X", "Y")
  )
  expect_error(
    collapse_episode(df, clnt_id = clnt, start_dt = segment_start, gap = 30, .dt_trans = NULL),
    "conflict"
  )
})

test_that("overwrite switches between gap and gap_overwrite", {
  # A,A are ~50 days apart (> gap); B follows A
  df <- data.frame(
    clnt = 1L,
    segment_start = as.Date(c("2020-01-01", "2020-03-01", "2020-06-01")),
    segment_end = as.Date(c("2020-01-10", "2020-03-10", "2020-06-10")),
    rx = c("A", "A", "B")
  )

  # same rx (A,A) collapsed under the relaxed gap_overwrite; the rx switch
  # (A -> B) falls back to the regular gap and starts a new episode
  out <- collapse_episode(df,
    clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
    gap = 10, overwrite = rx, gap_overwrite = 999, .dt_trans = NULL
  )
  expect_equal(out$epi_no, c(1, 1, 2))

  # a tighter custom gap_overwrite the same-rx records now exceed forces a split
  out2 <- collapse_episode(df,
    clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
    gap = 10, overwrite = rx, gap_overwrite = 20, .dt_trans = NULL
  )
  expect_equal(out2$epi_no, c(1, 2, 3))

  # the database backend agrees on the relaxed case
  out_db <- collapse_episode(memdb_tbl(df),
    clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
    gap = 10, overwrite = rx, gap_overwrite = 999
  ) %>%
    dplyr::collect() %>%
    dplyr::arrange(segment_start)
  expect_equal(out_db$epi_no, c(1, 1, 2))
})

test_that("records with missing dates are dropped with a warning", {
  # the NA end date used to silently merge every later record into one episode
  df <- data.frame(
    clnt = 1L,
    segment_start = as.Date(c("2020-01-01", "2020-01-05", "2020-01-20", "2020-01-25")),
    segment_end = as.Date(c("2020-01-10", NA, "2020-01-22", "2020-01-30"))
  )

  expect_warning(
    out <- collapse_episode(df,
      clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
      gap = 2, .dt_trans = NULL
    ),
    "missing"
  )
  expect_equal(nrow(out), 3)
  expect_equal(out$epi_no, c(1, 2, 3))

  expect_warning(
    out_db <- collapse_episode(memdb_tbl(df),
      clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
      gap = 2
    ) %>%
      dplyr::collect(),
    "missing"
  )
  expect_equal(nrow(out_db), 3)
  expect_equal(sort(out_db$epi_no), c(1, 2, 3))

  # a missing start date is dropped on the start-only path too
  df2 <- data.frame(clnt = 1L, segment_start = as.Date(c("2020-01-01", NA, "2020-06-01")))
  expect_warning(
    out2 <- collapse_episode(df2, clnt_id = clnt, start_dt = segment_start, gap = 2, .dt_trans = NULL),
    "missing"
  )
  expect_equal(nrow(out2), 2)
  expect_equal(out2$epi_no, c(1, 2))
})

test_that("data.frame and database backends produce identical grouping", {
  set.seed(123)
  df <- data.frame(
    clnt = rep(1:5, each = 5),
    # sample without replacement -> starts are unique within each client,
    # so the within-episode ordering is unambiguous across backends
    segment_start = as.Date("2020-01-01") + sample(0:300, 25, replace = FALSE)
  )
  df <- df[order(df$clnt, df$segment_start), ]

  out_df <- collapse_episode(df, clnt_id = clnt, start_dt = segment_start, gap = 14, .dt_trans = NULL) %>%
    dplyr::arrange(clnt, segment_start)
  out_db <- collapse_episode(memdb_tbl(df), clnt_id = clnt, start_dt = segment_start, gap = 14) %>%
    dplyr::collect() %>%
    dplyr::arrange(clnt, segment_start)

  expect_equal(out_db$epi_no, out_df$epi_no)
  expect_equal(out_db$epi_seq, out_df$epi_seq)
  expect_equal(out_db$epi_id, out_df$epi_id)
})

test_that("start > end triggers a warning", {
  df <- data.frame(
    clnt = 1L,
    segment_start = as.Date(c("2020-01-01", "2020-02-10")),
    segment_end = as.Date(c("2020-01-31", "2020-02-05"))
  )
  expect_warning(
    collapse_episode(df,
      clnt_id = clnt, start_dt = segment_start, end_dt = segment_end,
      gap = 5, .dt_trans = NULL
    ),
    "start > end"
  )
})

test_that("single-row and empty input are handled", {
  one <- data.frame(clnt = 1L, segment_start = as.Date("2020-01-01"))
  out <- collapse_episode(one, clnt_id = clnt, start_dt = segment_start, gap = 30, .dt_trans = NULL)
  expect_equal(out$epi_id, 1L)
  expect_equal(out$epi_no, 1L)
  expect_equal(out$epi_seq, 1L)

  empty <- data.frame(clnt = integer(0), segment_start = as.Date(character(0)))
  expect_no_warning(
    out_empty <- collapse_episode(empty, clnt_id = clnt, start_dt = segment_start, gap = 30, .dt_trans = NULL)
  )
  expect_equal(nrow(out_empty), 0L)
  expect_true(all(c("epi_id", "epi_no", "epi_seq", "epi_start_dt", "epi_stop_dt") %in% names(out_empty)))
  expect_s3_class(out_empty$epi_start_dt, "Date")
})
