mixed_case_dat <- function() {
  dplyr::tibble(
    clnt_id = 1:4,
    diagx = c("f10", "F10", "f20", "F20")
  )
}

test_that("'like'/'start' ignore case by default on both backends", {
  df <- mixed_case_dat()

  for (dat in list(df, memdb_tbl(df))) {
    for (m in c("start", "like")) {
      vals <- if (m == "start") "f1" else "f1%"
      out <- identify_rows(dat, diagx, m, vals, query_only = FALSE)
      expect_setequal(out$diagx, c("f10", "F10"))

      # an upper-case pattern must find the same records
      vals_up <- toupper(vals)
      out_up <- identify_rows(dat, diagx, m, vals_up, query_only = FALSE)
      expect_setequal(out_up$diagx, c("f10", "F10"))
    }
  }
})

test_that("ignore_case = FALSE restores a case-sensitive match", {
  df <- mixed_case_dat()

  # the data.frame backend is case-sensitive with the flag off; remote tables
  # are not asserted here because a bare LIKE follows the database (and its
  # collation), which is exactly what ignore_case = TRUE exists to avoid
  for (m in c("start", "like")) {
    vals <- if (m == "start") "f1" else "f1%"
    out <- identify_rows(df, diagx, m, vals, ignore_case = FALSE)
    expect_equal(out$diagx, "f10")
  }
})

test_that("ignore_case reaches identify_row through define_case", {
  df <- mixed_case_dat()

  out <- define_case(df, diagx, "start", "f1", clnt_id = clnt_id, mode = "filter")
  expect_setequal(out$diagx, c("f10", "F10"))

  out_cs <- define_case(df, diagx, "start", "f1",
    clnt_id = clnt_id, mode = "filter", ignore_case = FALSE
  )
  expect_equal(out_cs$diagx, "f10")
})

test_that("verbose reports case sensitivity for 'like'/'start' only", {
  df <- mixed_case_dat()

  for (dat in list(df, memdb_tbl(df))) {
    expect_message(
      identify_rows(dat, diagx, "start", "f1", verbose = TRUE),
      "ignoring case.*ignore_case = FALSE.*faster"
    )
    expect_message(
      identify_rows(dat, diagx, "start", "f1", ignore_case = FALSE, verbose = TRUE),
      "matching case exactly"
    )
    # match types that ignore_case does not apply to say nothing about case
    expect_no_match(
      capture_messages(identify_rows(dat, diagx, "in", "f10", verbose = TRUE)),
      "case",
      all = TRUE
    )
  }
})

test_that("ignore_case does not affect the other match types", {
  df <- mixed_case_dat()

  # "in" compares values as they are
  expect_equal(identify_rows(df, diagx, "in", "f10")$diagx, "f10")
  # "regex" follows the supplied pattern
  expect_equal(identify_rows(df, diagx, "regex", "^f1")$diagx, "f10")
  expect_setequal(identify_rows(df, diagx, "regex", "(?i)^f1")$diagx, c("f10", "F10"))
})
