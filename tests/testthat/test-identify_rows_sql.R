test_that("match by in works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, starts_with("diagx"), "in", letters, query_only = FALSE)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
  # also test cat output
  expect_message(identify_rows(db, starts_with("diagx"), "in", letters, verbose = TRUE))
})

test_that("if_all works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, if_all = TRUE, starts_with("diagx"), "in", letters, query_only = FALSE)
  expect_equal(out_df, subset(df, ans == "all"), ignore_attr = "row.names")
})

test_that("match by start works", {
  start <- "f"
  db <- xnum_n(x = start, type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, starts_with("diagx"), "start", start, query_only = FALSE)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("match by like works", {
  start <- paste0("F", 1:9) %>% rep(each = 9)
  pattern <- paste0("F", 1:9, "%")
  db <- xnum_n(x = start, type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(uid)
  out_df <- identify_rows(db, starts_with("diagx"), "like", pattern, query_only = FALSE)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("match by regex works", {
  start <- paste0("F", 1:9) %>% rep(each = 9)
  pattern <- "F[1-9][1-9]"
  db <- xnum_n(x = start, type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, starts_with("diagx"), "regex", pattern, query_only = FALSE)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
  # also test one var works for pivot
  expect_no_error(identify_rows(db, diagx, "regex", pattern, query_only = FALSE))
})

test_that("match by between works", {
  ans <- 5
  date_seq <- seq(as.Date("2021-01-01"), as.Date("2021-01-31"), by = 1)
  seq_range <- range(date_seq) %>% as.numeric()
  db <- btw_n(date_range = date_seq, n_ans = ans, type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, dates, "between", seq_range, query_only = FALSE)
  expect_equal(out_df %>% nrow(), ans)
})

test_that("match by glue_sql works", {
  start <- "f"
  pattern <- "{`vars`} IN ({values*})"
  db <- xnum_n(x = start, type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, starts_with("diagx"), "glue_sql", pattern, values = paste0("f", 1:9), query_only = FALSE)
  expect_equal(out_df, subset(df, ans %in% c("all", "any")), ignore_attr = "row.names")
})

test_that("type conflict warning works", {
  db <- letters_n(type = "database")
  expect_warning(identify_rows(db, starts_with("diagx"), "in", 1:10), "not the same type")
})

test_that("edge case - character vars works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  var_nm <- c("diagx", "diagx_1", "diagx_2")
  out_df <- identify_rows(db, any_of(var_nm), "in", letters, query_only = FALSE)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})

test_that("edge case - unquoted vars works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  out_df <- identify_rows(db, c(diagx, diagx_1, diagx_2), "in", letters, query_only = FALSE)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})

test_that("edge case - vals in an external vector works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  val <- letters
  out_df <- identify_rows(db, c(diagx, diagx_1, diagx_2), "in", val, query_only = FALSE)
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})
