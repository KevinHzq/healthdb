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

test_that("multiple def works", {
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
  def2 <- build_def("SUD2",
                   src_labs = c("msp_b", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id
                   ))
  def <- dplyr::bind_rows(def, def2)
  out_df <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df, msp_b = msp_df), bind = TRUE)
  expect_gt(nrow(out_df), 0)
})

test_that("execute_def works with define_case_with_age and age column", {
  msp_df <- letters_n()
  msp_df$age <- sample(10:80, nrow(msp_df), replace = TRUE)
  dad_df <- xnum_n("F")
  dad_df$age <- sample(10:80, nrow(dad_df), replace = TRUE)

  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = define_case_with_age,
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     age = age,
                     age_range = list(c(18, 65)),
                     mode = "filter"
                   ))
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")
  # verify age filtering worked
  expect_true(all(result[[1]]$age >= 18 & result[[1]]$age <= 65))
  expect_true(all(result[[2]]$age >= 18 & result[[2]]$age <= 65))
})

test_that("execute_def works with define_case_with_age and birth_date", {
  msp_df <- letters_n()
  msp_df$birth_dt <- as.Date("1990-01-01")
  dad_df <- xnum_n("F")
  dad_df$birth_dt <- as.Date("1980-01-01")

  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = define_case_with_age,
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     date_var = dates,
                     birth_date = birth_dt,
                     age_range = list(c(18, 65)),
                     mode = "filter"
                   ))
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")
  # verify data frames have birth_dt column
  expect_true("birth_dt" %in% names(result[[1]]))
  expect_true("birth_dt" %in% names(result[[2]]))
})

test_that("execute_def binding works with define_case_with_age", {
  msp_df <- letters_n()
  msp_df$age <- sample(10:80, nrow(msp_df), replace = TRUE)
  dad_df <- letters_n()
  dad_df$age <- sample(10:80, nrow(dad_df), replace = TRUE)

  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = define_case_with_age,
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = letters,
                     clnt_id = clnt_id,
                     age = age,
                     age_range = list(c(18, 65)),
                     mode = "filter"
                   ))
  out_df <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df), bind = TRUE)
  expect_s3_class(out_df, "data.frame")
  expect_true(all(out_df$age >= 18 & out_df$age <= 65))
  expect_gt(nrow(out_df), 0)
})

test_that("execute_def with mixed define_case_with_age and identify_rows works", {
  msp_df <- letters_n()
  msp_df$age <- sample(10:80, nrow(msp_df), replace = TRUE)
  dad_df <- letters_n()

  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = list(define_case_with_age, identify_rows),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = letters,
                     clnt_id = clnt_id,
                     age = age,
                     age_range = list(c(18, 65))
                   ))
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")
  # msp should have age filtering applied
  expect_true(all(result[[1]]$age >= 18 & result[[1]]$age <= 65))
  # dad from identify_rows should not be filtered (no age restriction)
  expect_true(nrow(result[[2]]) > 0)
})

test_that("execute_def with different age_range per source works", {
  msp_df <- letters_n()
  msp_df$age <- sample(10:80, nrow(msp_df), replace = TRUE)
  dad_df <- letters_n()
  dad_df$age <- sample(10:80, nrow(dad_df), replace = TRUE)

  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = define_case_with_age,
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = letters,
                     clnt_id = clnt_id,
                     age = age,
                     age_range = list(c(18, 65), c(21, NA)),
                     mode = "filter"
                   ))
  result <- execute_def(def, with_data = list(msp = msp_df, dad = dad_df))
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")
  # msp should be filtered for 18-65
  expect_true(all(result[[1]]$age >= 18 & result[[1]]$age <= 65))
  # dad should be filtered for 21+
  expect_true(all(result[[2]]$age >= 21))
})

