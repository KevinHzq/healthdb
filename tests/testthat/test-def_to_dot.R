test_that("basic use works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     uid = uid,
                     n_per_clnt = c(1, 1),
                     #date_var = dates,
                     date_var = c("serv_date", "sep_date"),
                     apart = c(NULL, NULL),
                     within = c(365, NULL)
                   ))
  out_list <- def_to_dot(def)
  ans_list <- list(
    def = "def",
    src = "src",
    uid = c("uid", "uid"),
    clnt_id = c("clnt_id", "clnt_id"),
    date_var = c("serv_date", "sep_date"),
    #flag_restrict_n = c(NA, NA),
    flag_restrict_dates = c("flag_restrict_dates", NA)
  )
  expect_equal(out_list, ans_list)
  # test def_fn warning
  def <- build_def("OUD", src_labs = "msp", def_fn = identify_rows, fn_args = list(vars = abc))
  expect_warning(def_to_dot(def), "define_case")
})

test_that("presence of mode works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     uid = uid,
                     n_per_clnt = c(2, 1),
                     mode = c("filter", "flag"),
                     date_var = c("serv_date", "sep_date"),
                     apart = c(NULL, NULL),
                     within = c(365, 365)
                   ))
  out_list <- def_to_dot(def)
  ans_list <- list(
    def = "def",
    src = "src",
    uid = c("uid", "uid"),
    clnt_id = c("clnt_id", "clnt_id"),
    date_var = c("serv_date", "sep_date"),
    flag_restrict_n = c("flag_restrict_n", NA),
    flag_restrict_dates = c("flag_restrict_dates", "flag_restrict_dates")
  )
  expect_equal(out_list, ans_list)
})

test_that("one date_var works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     uid = uid,
                     n_per_clnt = c(2, 1),
                     mode = c("filter", "flag"),
                     date_var = list(serv_date, NULL),
                     apart = c(NULL, NULL),
                     within = c(NULL, 365)
                   ))
  out_list <- def_to_dot(def)
  ans_list <- list(
    def = "def",
    src = "src",
    uid = c("uid", "uid"),
    clnt_id = c("clnt_id", "clnt_id"),
    date_var = c("serv_date", NA),
    flag_restrict_n = c("flag_restrict_n", NA),
    flag_restrict_dates = c(NA, "flag_restrict_dates")
  )
  expect_equal(out_list, ans_list)
})

test_that("two def works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters, "F"),
                     clnt_id = clnt_id,
                     uid = uid,
                     n_per_clnt = c(1, 1),
                     #date_var = dates,
                     date_var = c("serv_date", "sep_date"),
                     apart = c(NULL, NULL)
                   ))
  def2 <- build_def("SUD2",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     match = "start",
                     vals = list(letters[1:10], "F1"),
                     clnt_id = clnt_id,
                     uid = uid,
                     n_per_clnt = c(2, 1),
                     align = "right",
                     date_var = c("serv_date", "sep_date"),
                     apart = c(NULL, NULL),
                     within = c(NULL, 365)
                   ))
  def <- dplyr::bind_rows(def, def2)
  out_list <- def_to_dot(def)
  ans_list <- list(
    def = "def",
    src = "src",
    uid = c("uid", "uid", "uid", "uid"),
    clnt_id = c("clnt_id", "clnt_id", "clnt_id", "clnt_id"),
    date_var = c("serv_date", "sep_date", "serv_date", "sep_date"),
    flag_restrict_n = c(NA, NA, "flag_restrict_n", NA),
    flag_restrict_dates = c(NA, NA, NA, "flag_restrict_dates")
  )
  expect_equal(out_list, ans_list)
})

# test_that("stop if different var names for same source works", {
#   def <- build_def("SUD",
#                    src_labs = c("msp", "dad"),
#                    fn_args = list(
#                      vars = starts_with("diagx"),
#                      match = "start",
#                      vals = list(letters, "F"),
#                      clnt_id = clnt_id,
#                      uid = uid,
#                      n_per_clnt = c(1, 1),
#                      #date_var = dates,
#                      date_var = c("serv_date", "sep_date"),
#                      apart = c(NULL, NULL)
#                    ))
#   def2 <- build_def("SUD2",
#                     src_labs = c("msp", "dad"),
#                     fn_args = list(
#                       vars = starts_with("diagx"),
#                       match = "start",
#                       vals = list(letters[1:10], "F1"),
#                       clnt_id = clnt_id,
#                       uid = uid,
#                       n_per_clnt = c(2, 1),
#                       align = "right",
#                       date_var = "sep_date",
#                       apart = c(NULL, NULL),
#                       within = c(NULL, 365)
#                     ))
#   def <- dplyr::bind_rows(def, def2)
#   def_to_dot(def)
#   expect_error(def_to_dot(def), "multiple ID names")
# })
