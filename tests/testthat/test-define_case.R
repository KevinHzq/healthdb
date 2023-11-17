test_that("identify-only works", {
  db <- letters_n(type = "database")
  output_df <- define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", force_collect = TRUE)
  expect_s3_class(output_df, "data.frame")
})

test_that("identify+exclude works", {
  excl_vals <- c("a", "b")
  db <- letters_n(type = "database")
  output_df <- define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", excl_vals = excl_vals, force_collect = TRUE)
  expect_s3_class(output_df, "data.frame")
})

test_that("identify+restrict_n works", {
  db <- letters_n(type = "database")
  output_df <- define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", n_per_clnt = 2, force_collect = TRUE)
  expect_s3_class(output_df, "data.frame")
})

test_that("identify+exclude+restrict_n works", {
  excl_vals <- c("a", "b")
  db <- letters_n(type = "database")
  output_df <- define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", excl_vals = excl_vals, n_per_clnt = 2, force_collect = TRUE)
  expect_s3_class(output_df, "data.frame")
})

test_that("identify+restrict_n+restrict_dates works", {
  db <- letters_n(type = "database")
  output_df <- define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", n_per_clnt = 2, date_var = dates, apart = 2, within = 365, uid = uid, force_collect = TRUE)
  expect_s3_class(output_df, "data.frame")
})

test_that("output is correct for database", {
  skip_on_cran()
  n <- 2
  apart <- 30
  within <- 365
  db <- make_test_dat(type = "database", answer_id = "ans")
  df <- dplyr::collect(db)
  ans_df <- df %>% dplyr::filter(ans != "noise")
  out_df <- df %>% dplyr::filter(dplyr::if_any(dplyr::starts_with("diagx"), ~ . %in% "999"))
  ans_df <- ans_df %>% dplyr::anti_join(out_df, by = "clnt_id")
  ans_id <- test_apart_within(ans_df, n, apart, within)
  output_df <- define_case(db, starts_with("diagx"), "start", c("304", "305"), excl_vals = "999", clnt_id = clnt_id, mode = "filter", n_per_clnt = n, date_var = dates, apart = apart, within = within, force_collect = TRUE)
  expect_setequal(output_df$clnt_id, ans_id)
  # also test mode
  output_df_flag <- define_case(db, starts_with("diagx"), "start", c("304", "305"), excl_vals = "999", clnt_id = clnt_id, n_per_clnt = n, date_var = dates, apart = apart, within = within, mode = "flag", force_collect = TRUE)
  expect_gt(nrow(output_df_flag), nrow(output_df))
  expect_setequal(dplyr::filter(output_df_flag, dplyr::if_all(dplyr::starts_with("flag_"), ~ . == 1)) %>% dplyr::pull(clnt_id), ans_id)
  #also test df input
  output_df2 <- define_case(df, starts_with("diagx"), "start", c("304", "305"), excl_vals = "999", clnt_id = clnt_id, mode = "filter", n_per_clnt = n, date_var = dates, apart = apart, within = within)
  expect_setequal(output_df2$clnt_id, ans_id)
})

# test_that("SQL slice_max translation works", {
#   db <- letters_n(type = "database")
#   df <- db %>% dplyr::group_by(clnt_id) %>% dplyr::slice_max(dates, n = 1, with_ties = FALSE) %>% dplyr::collect()
#   expect_s3_class(df, "data.frame")
# })

test_that("keep first/last works", {
  db <- letters_n(type = "database", id = 1:10)
  df_list <- sapply(c("all", "first", "last"), function(x) define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", date_var = dates, keep = x, force_collect = TRUE), USE.NAMES = TRUE, simplify = FALSE)
  expect_true(nrow(df_list[["all"]]) > nrow(df_list[["first"]]))
  expect_true(nrow(df_list[["all"]]) > nrow(df_list[["last"]]))
  expect_s3_class(df_list[["first"]], "data.frame")
  expect_s3_class(df_list[["last"]], "data.frame")
  # also check missing date_var
  expect_error(define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", keep = "last"), "must be supplied")
  expect_error(define_case(db, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", within = 365), "must be supplied")
})

test_that("keep first/last works on df", {
  df <- letters_n(type = "data.frame", id = 1:10)
  df_list <- sapply(c("all", "first", "last"), function(x) define_case(df, starts_with("diagx"), "in", letters, clnt_id = clnt_id, mode = "filter", date_var = dates, keep = x, force_collect = TRUE), USE.NAMES = TRUE, simplify = FALSE)
  expect_true(nrow(df_list[["all"]]) > nrow(df_list[["first"]]))
  expect_true(nrow(df_list[["all"]]) > nrow(df_list[["last"]]))
  expect_s3_class(df_list[["first"]], "data.frame")
  expect_s3_class(df_list[["last"]], "data.frame")
})

test_that("passing ... works", {
  df <- make_test_dat()
  output_l <- define_case(df, starts_with("diagx"), "start", c("304"), clnt_id = clnt_id, mode = "filter", n_per_clnt = 2, date_var = dates, apart = 2, within = 365, uid = uid, force_collect = TRUE, align = "left")
  output_r <- define_case(df, starts_with("diagx"), "start", c("304"), clnt_id = clnt_id, mode = "filter", n_per_clnt = 2, date_var = dates, apart = 2, within = 365, uid = uid, force_collect = TRUE, align = "right")
  expect_false(identical(output_l, output_r))
})
