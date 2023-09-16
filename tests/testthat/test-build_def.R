test_that("basic use works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = letters
                   ))
  if_call <- purrr::map_lgl(def$fn_call, rlang::is_call)
  expect_true(all(if_call))
})

test_that("edge case - arg is external symbol works", {
  def_lab <- "SUD"
  srcs <- c("msp", "dad")
  some_vals <- 1:10
  def <- build_def(def_lab,
                   src_labs = srcs,
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = some_vals
                   ))
  expect_equal(def$def_lab, rep(def_lab, 2))
  expect_equal(def$src_labs, srcs)
  expect_true(stringr::str_detect(deparse(def$fn_call[[1]]), "some_vals"))
})

test_that("removal of invalid arguments works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     what = what
                   ))
  if_call <- purrr::map_lgl(def$fn_call, rlang::is_call)
  if_invalid <- purrr::map_lgl(def$fn_call, ~ stringr::str_detect(deparse(.), "what"))
  expect_true(all(if_call))
  expect_false(any(if_invalid))
  # also test ... in fn_args
  expect_error(build_def("SUD",
                         src_labs = c("msp", "dad"),
                         fn_args = list(
                           vars = starts_with("diagx"),
                           ... = what
                         )), "...")
})

test_that("multiple def_fn works", {
  def <- build_def("SUD",
                   src_labs = c("msp", "dad"),
                   def_fn = list(define_case, identify_rows),
                   fn_args = list(
                     vars = starts_with("diagx"),
                     vals = letters
                   ))
  if_call <- purrr::map_lgl(def$fn_call, rlang::is_call)
  expect_true(all(if_call))
})

test_that("incorrect recycling does not work", {
  expect_error(build_def("SUD",
                         src_labs = c("msp", "dad"),
                         fn_args = list(
                           vars = starts_with("diagx"),
                           vals = list(1, 2, 3)
                         )), "recycle")
})


