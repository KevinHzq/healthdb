test_that("pool databases works", {
  db <- make_test_dat(answer_id = "ans", type = "database")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "flag"),
      vals = c(304, 305),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(3, 1),
      within = c(NULL, NULL)
    )
  )
  result <- execute_def(def, with_data = list(msp = db, dad = db))
  pool_result <- pool_case(result, def, output_lvl = "clnt", include_src = "n_per_clnt") %>%
    # test compatibility with compute()
    dplyr::compute() %>%
    dplyr::collect()
  expect_gt(nrow(pool_result), 0)
  # also test include_src works
  pool_result2 <- pool_case(result, def, output_lvl = "clnt", include_src = "all") %>% dplyr::collect()
  expect_true(all(pool_result2$last_entry_date >= pool_result$last_entry_date))
})

test_that("pool mixed works", {
  msp_df <- letters_n()
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "filter"),
      vals = list(letters, "F"),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(2, 1),
      within = c(365, NULL)
    )
  )
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt")
  expect_gt(nrow(pool_result), 0)
})

test_that("edge case no date_var works", {
  msp_df <- letters_n()
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "filter"),
      vals = list(letters, "F"),
      clnt_id = clnt_id
      # , date_var = dates
    )
  )
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt")
  expect_gt(nrow(pool_result), 0)
})

test_that("edge case one date_var works", {
  msp_df <- letters_n()
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "filter"),
      vals = list(letters, "F"),
      clnt_id = clnt_id,
      date_var = c(dates, NULL)
    )
  )
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_warning(pool_result <- pool_case(result, def, output_lvl = "clnt"), "date_var")
  expect_gt(nrow(pool_result), 0)
  # also test df raw level
  expect_warning(pool_result <- pool_case(result, def, output_lvl = "raw"), "date_var")
})

test_that("formular generation and translation works", {
  msp_db <- letters_n(type = "database")
  src_nm <- c("noise", "any")
  # nml <- purrr::map(src_nm, function(x) glue::glue('~ . == "{x}"') %>% as.formula())
  nml <- purrr::map(src_nm, function(x) rlang::new_formula(NULL, rlang::expr(. == !!x)))
  names(nml) <- glue::glue("is_{src_nm}")
  out_df <- msp_db %>%
    dplyr::mutate(dplyr::across(ans, list(!!!nml), .names = "{.fn}")) %>%
    dplyr::collect() %>%
    dplyr::arrange(uid)
  ans <- msp_db %>%
    dplyr::arrange(uid) %>%
    dplyr::pull(ans)
  # as.numeric on both sides as the indicator type varies by backend
  # (e.g., logical from SQL Server BIT, numeric from SQLite)
  expect_equal(as.numeric(out_df$is_any), as.numeric(ans == "any"))
})

test_that("pool multiple def works", {
  msp_df <- letters_n()
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "filter"),
      vals = list(letters, "F"),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(2, 1),
      within = c(365, NULL)
    )
  )
  def2 <- build_def("SUD2",
    src_labs = c("msp", "dad2"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "filter"),
      vals = list(letters, "F"),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(3, 1)
    )
  )
  def <- dplyr::bind_rows(def, def2)
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df, dad2 = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt")
  expect_gt(nrow(pool_result), 0)
})

test_that("include has_valid works", {
  db <- make_test_dat(answer_id = "ans", type = "database")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "flag"),
      vals = c(304, 305),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(3, 1),
      within = c(NULL, NULL)
    )
  )
  result <- execute_def(def, with_data = list(msp = db, dad = db))
  pool_result <- pool_case(result, def, output_lvl = "raw", include_src = "has_valid") %>% dplyr::collect()
  expect_gt(nrow(pool_result), 0)
  # also test include_src works
  pool_result2 <- pool_case(result, def, output_lvl = "raw", include_src = "all") %>% dplyr::collect()
  expect_true(nrow(pool_result2) >= nrow(pool_result))
})

test_that("edge case full flag works", {
  db <- make_test_dat(answer_id = "ans", type = "database")
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = starts_with("diagx"),
      match = "start",
      mode = c("flag", "flag"),
      vals = c(304, 305),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(3, 2)
    )
  )
  result <- execute_def(def, with_data = list(msp = db, dad = db))
  pool_result <- pool_case(result, def, output_lvl = "raw", include_src = "has_valid") %>% dplyr::collect()
  expect_gt(nrow(pool_result), 0)
})

test_that("first/last entry summaries do not depend on the input row order", {
  # clnt 1: dad has only 2 records, so it fails n_per_clnt = 3 and holds no valid
  #   record, but its records still bracket the msp ones in time.
  # clnt 2: both sources are valid and share the earliest and the latest date, so
  #   the source is decided by the alphabetical tie-break.
  msp_df <- data.frame(
    clnt_id = c(1, 1, 2, 2, 2),
    uid = 1:5,
    dates = as.Date(c("2020-05-01", "2020-09-01", "2020-02-01", "2020-06-01", "2020-08-01")),
    diagx = "304a"
  )
  dad_df <- data.frame(
    clnt_id = c(1, 1, 2, 2, 2),
    uid = 6:10,
    dates = as.Date(c("2020-03-01", "2020-12-01", "2020-02-01", "2020-07-01", "2020-08-01")),
    diagx = "305a"
  )
  def <- build_def("SUD",
    src_labs = c("msp", "dad"),
    fn_args = list(
      vars = "diagx",
      match = "start",
      mode = c("flag", "flag"),
      vals = list("304", "305"),
      clnt_id = clnt_id,
      uid = uid,
      date_var = dates,
      n_per_clnt = c(2, 3)
    )
  )

  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt", include_src = "all")

  expect_equal(pool_result$clnt_id, c(1, 2))
  # the earliest record of a client is not necessarily one that made them a case
  expect_equal(pool_result$first_entry_date, as.Date(c("2020-03-01", "2020-02-01")))
  expect_equal(pool_result$first_entry_src, c("dad", "dad"))
  expect_equal(pool_result$first_valid_date, as.Date(c("2020-05-01", "2020-02-01")))
  expect_equal(pool_result$first_valid_src, c("msp", "dad"))
  expect_equal(pool_result$last_entry_date, as.Date(c("2020-12-01", "2020-08-01")))
  expect_equal(pool_result$last_entry_src, c("dad", "msp"))
  # clnt 1's two dad records fail n_per_clnt = 3, but clnt 2's three do not
  expect_equal(as.numeric(pool_result$valid_in_dad), c(0, 3))

  # shuffling the input must not change any of the above
  set.seed(42)
  result_shuffled <- execute_def(def, with_data = list(
    msp = msp_df[sample(nrow(msp_df)), ],
    dad = dad_df[sample(nrow(dad_df)), ]
  ))
  expect_equal(
    pool_case(result_shuffled, def, output_lvl = "clnt", include_src = "all"),
    pool_result
  )

  # and the database backend must agree with the data.frame one
  result_db <- execute_def(def, with_data = list(msp = memdb_tbl(msp_df), dad = memdb_tbl(dad_df)))
  pool_db <- pool_case(result_db, def, output_lvl = "clnt", include_src = "all") %>%
    dplyr::collect() %>%
    dplyr::arrange(clnt_id)
  as_date <- function(x) if (is.numeric(x)) as.Date(x, origin = "1970-01-01") else as.Date(x)
  expect_equal(as.numeric(pool_db$clnt_id), pool_result$clnt_id)
  expect_equal(as_date(pool_db$first_entry_date), pool_result$first_entry_date)
  expect_equal(pool_db$first_entry_src, pool_result$first_entry_src)
  expect_equal(as_date(pool_db$first_valid_date), pool_result$first_valid_date)
  expect_equal(pool_db$first_valid_src, pool_result$first_valid_src)
  expect_equal(as_date(pool_db$last_entry_date), pool_result$last_entry_date)
  expect_equal(pool_db$last_entry_src, pool_result$last_entry_src)
})

test_that("test window function behavior", {
  db <- memdb_tbl(mtcars)
  db1 <- db %>%
    dplyr::group_by(gear) %>%
    dbplyr::window_order(cyl) %>%
    dplyr::mutate(lag = lag(mpg))
  db2 <- db %>%
    dplyr::group_by(gear) %>%
    dbplyr::window_order(cyl) %>%
    dplyr::mutate(lag = lag(mpg)) %>%
    dbplyr::window_order()
  expect_identical(dbplyr::remote_query(db1), dbplyr::remote_query(db2))
  expect_true("order_vars" %in% names(db1[["lazy_query"]]))
  expect_false("order_vars" %in% names(db2[["lazy_query"]]))
})
