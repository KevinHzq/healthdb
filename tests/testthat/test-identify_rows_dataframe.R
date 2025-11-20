test_that("match by in works", {
  df <- letters_n(type = "data.frame")
  out_df <- identify_rows(df, starts_with("diagx"), "in", letters)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
  # also test cat output
  expect_output(identify_rows(df, starts_with("diagx"), "in", letters, verbose = TRUE))
})

test_that("if_all works", {
  df <- letters_n(type = "data.frame")
  out_df <- identify_rows(df, if_all = TRUE, starts_with("diagx"), "in", letters)
  expect_equal(out_df, subset(df, ans == "all"), ignore_attr = "row.names")
})

test_that("match by start works", {
  start <- "f"
  df <- xnum_n(x = start, type = "data.frame")
  out_df <- identify_rows(df, starts_with("diagx"), "start", start)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("match by like works", {
  start <- paste0("F", 1:9) %>% rep(each = 9)
  pattern <- paste0("F", 1:9, "%")
  df <- xnum_n(x = start, type = "data.frame")
  out_df <- identify_rows(df, starts_with("diagx"), "like", pattern)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("match by regex works", {
  start <- paste0("F", 1:9) %>% rep(each = 9)
  pattern <- "F[1-9][1-9]"
  df <- xnum_n(x = start, type = "data.frame")
  out_df <- identify_rows(df, starts_with("diagx"), "regex", pattern)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("match by between works", {
  ans <- 5
  date_seq <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = 1)
  seq_range <- range(date_seq)
  df <- btw_n(date_range = date_seq, n_ans = ans, type = "data.frame")
  out_df <- identify_rows(df, dates, "between", seq_range)
  expect_equal(out_df %>% nrow(), ans)
})

test_that("type conflict warning works", {
  df <- letters_n(type = "data.frame")
  expect_warning(identify_rows(df, starts_with("diagx"), "in", 1:10), "not the same type")
  # also test match cannot be glue_sql
  expect_error(identify_rows(df, starts_with("diagx"), "glue_sql", 1:10), "glue_sql")
})

test_that("summary of numeric vals works", {
  df <- make_test_dat(vals_kept = 1:10, noise_val = 99, nrows = 10, n_any = 10, n_all = 10)
  expect_output(identify_rows(df, starts_with("diagx"), "in", 1:10, verbose = TRUE), "Min.")
})

test_that("edge case - character vars works", {
  df <- letters_n(type = "data.frame")
  var_nm <- c("diagx", "diagx_1", "diagx_2")
  out_df <- identify_rows(df, any_of(var_nm), "in", letters)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})

test_that("edge case - unquoted vars works", {
  df <- letters_n(type = "data.frame")
  out_df <- identify_rows(df, c(diagx, diagx_1, diagx_2), "in", letters)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})

test_that("edge case - vals in an external vector works", {
  df <- letters_n(type = "data.frame")
  val <- letters
  out_df <- identify_rows(df, c(diagx, diagx_1, diagx_2), "in", val)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})
