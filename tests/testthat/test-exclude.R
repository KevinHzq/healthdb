test_that("anti-join on data.frames works", {
  df <- letters_n()
  df_noise <- df %>% dplyr::filter(ans == "noise")
  out_id <- exclude(df, df_noise, by = "clnt_id") %>% dplyr::pull(clnt_id)
  ans_id <- subset(df, !(clnt_id %in% df_noise$clnt_id)) %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, ans_id)
})

test_that("mixed sources check works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  df_noise <- df %>% dplyr::filter(ans == "noise")
  expect_error(exclude(db, df_noise, by = "clnt_id") %>% dplyr::pull(clnt_id), "Both data")
})

test_that("anti-join on database works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db)
  db_noise <- db %>% dplyr::filter(ans == "noise")
  out_id <- exclude(db, db_noise, by = "clnt_id") %>% dplyr::pull(clnt_id)
  ans_id <- subset(df, !(clnt_id %in% dplyr::pull(db_noise, clnt_id))) %>% dplyr::pull(clnt_id)
  expect_setequal(out_id, ans_id)
})

test_that("condition on data.frames works", {
  df <- letters_n()
  out_df <- exclude(df, condition = ans == "noise")
  expect_equal(out_df, subset(df, ans != "noise"), ignore_attr = "row.names")
})

test_that("condition on database works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(uid)
  out_db <- exclude(db, condition = ans == "noise") %>% dplyr::arrange(uid)
  expect_equal(out_db %>% dplyr::collect(), subset(df, ans != "noise"), ignore_attr = "row.names")
  # also test console output
  expect_message(exclude(db, condition = ans == "noise", verbose = TRUE), "Consider")
})

test_that("report_on on data.frames works", {
  df <- letters_n()
  expect_message(exclude(df, condition = ans == "noise", report_on = clnt_id, verbose = TRUE), "were excluded")
})

test_that("report_on on database works", {
  db <- letters_n(type = "database")
  on <- "clnt_id"
  expect_message(exclude(db, condition = ans == "noise", report_on = !!on, verbose = TRUE), "were excluded")
})

test_that("edge case - variable contains NA on database works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(uid)
  on <- "clnt_id"
  out_db <- exclude(db, condition = diagx_2 %in% !!letters) %>% dplyr::arrange(uid)
  expect_equal(out_db %>% dplyr::collect(), dplyr::filter(df, !(diagx_2 %in% !!letters)), ignore_attr = "row.names")
})

test_that("edge case - excluding == keeps NA works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(uid)
  on <- "clnt_id"
  out_db <- exclude(db, condition = diagx_2 == "999") %>% dplyr::arrange(uid)
  expect_equal(out_db %>% dplyr::collect(), dplyr::filter(df, diagx_2 != "999" | is.na(diagx_2)), ignore_attr = "row.names")
})

test_that("edge case - complex condition on database works", {
  db <- letters_n(type = "database")
  df <- dplyr::collect(db) %>% dplyr::arrange(uid)
  on <- "clnt_id"
  out_db <- exclude(db, condition = dplyr::if_any(dplyr::starts_with("diagx"), ~ . %in% !!letters)) %>% dplyr::arrange(uid)
  expect_equal(out_db %>% dplyr::collect(), dplyr::filter(df, !(dplyr::if_any(dplyr::starts_with("diagx"), ~ . %in% !!letters))), ignore_attr = "row.names")
})
