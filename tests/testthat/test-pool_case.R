test_that("pool databases works", {
  msp_db <- letters_n(type = "database")
  dad_df <- xnum_n("F", type = "database")
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
                   ))
  result <- execute_def(def, with_data = list(msp = msp_db, dad = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt") %>% dplyr::collect(cte = TRUE)
  expect_gt(nrow(pool_result), 0)
  # also test valid_src_only
  pool_result2 <- pool_case(result, def, output_lvl = "clnt", valid_src_only = FALSE) %>% dplyr::collect(cte = TRUE)
  expect_true(any(pool_result2$last_entry_date > pool_result$last_entry_date))
  expect_false(any(pool_result2$last_entry_date < pool_result$last_entry_date))
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
                   ))
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
                   ))
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
                     clnt_id = clnt_id
                     , date_var = c(dates, NULL)
                   ))
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_warning(pool_result <- pool_case(result, def, output_lvl = "clnt"), "date_var")
  expect_gt(nrow(pool_result), 0)
})

test_that("formular generation and translation works", {
  msp_db <- letters_n(type = "database")
  src_nm <- c("noise", "any")
  # nml <- purrr::map(src_nm, function(x) glue::glue('~ . == "{x}"') %>% as.formula())
  nml <- purrr::map(src_nm, function(x) rlang::new_formula(NULL, rlang::expr(. == !!x)))
  names(nml) <- glue::glue("is_{src_nm}")
  out_df <- msp_db %>% dplyr::mutate(dplyr::across(ans, list(!!!nml), .names = "{.fn}")) %>% dplyr::collect()
  ans <- msp_db %>% dplyr::pull(ans)
  expect_equal(out_df$is_any, as.numeric(ans == "any"))
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
                   ))
  def2 <- build_def("SUD2",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     mode = c("flag", "filter"),
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     uid = uid,
                     date_var = dates,
                     n_per_clnt = c(3, 1)
                   ))
  def <- dplyr::bind_rows(def, def2)
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  pool_result <- pool_case(result, def, output_lvl = "clnt")
  expect_gt(nrow(pool_result), 0)
})
