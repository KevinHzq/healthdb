test_that("basic use works", {
  msp_db <- letters_n(type = "database")
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  result <- execute_def(def, with_data = list(msp = msp_db, dad = dad_df))
  expect_s3_class(result[[1]], "tbl_sql")
  expect_s3_class(result[[2]], "data.frame")

  #also test list object input works
  dat_list <- list(msp = msp_db, dad = dad_df)
  result <- execute_def(def, with_data = dat_list)
  expect_s3_class(result[[1]], "tbl_sql")
  expect_s3_class(result[[2]], "data.frame")
})

test_that("mixed binding works", {
  msp_db <- letters_n(type = "database")
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  # this give a warning for incompatible types
  expect_warning(execute_def(def, with_data = list(msp = msp_db, dad = dad_df), bind = TRUE), "incompatible")
  dad_df <- dad_df %>% dplyr::mutate(dates = as.numeric(dates))
  out_df <- execute_def(def, with_data = list(msp = msp_db, dad = dad_df), bind = TRUE)
  expect_s3_class(out_df, "data.frame")
})

test_that("binding dbs works", {
  msp_db <- letters_n(type = "database")
  dad_db <- xnum_n("F", type = "database")
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  expect_s3_class(execute_def(def, with_data = list(msp = msp_db, dad = dad_db), bind = TRUE), "tbl_sql")
})

test_that("binding dfs works", {
  msp_df <- letters_n()
  dad_df <- xnum_n("F")
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  expect_s3_class(execute_def(def, with_data = list(msp = msp_df, dad = dad_df), bind = TRUE), "data.frame")
})

test_that("n sources check works", {
  msp_df <- letters_n()
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  expect_error(execute_def(def, with_data = list(msp = msp_df), bind = TRUE), "sources")
})
