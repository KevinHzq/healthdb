test_that("data.frame input works", {
  df <- letters_n()
  expect_type(report_n(df, on = clnt_id), "integer")
})

test_that("database input works", {
  db <- letters_n(type = "database")
  expect_type(report_n(db, on = clnt_id), "integer")
})

test_that("multiple data.frame input works", {
  df <- letters_n()
  expect_type(report_n(df, df, on = clnt_id), "integer")
})

test_that("multiple database input works", {
  db <- letters_n(type = "database")
  expect_type(report_n(db, db, on = clnt_id), "integer")
})

test_that("mixed input works", {
  db <- letters_n(type = "database")
  df <- letters_n()
  expect_type(report_n(db, df, on = clnt_id), "integer")
})

test_that("column name check works", {
  db <- letters_n(type = "database")
  expect_error(report_n(db, mtcars, on = clnt_id))
})

test_that("empty dots check works", {
  db <- letters_n(type = "database")
  expect_error(report_n(on = clnt_id), "No data")
})

test_that("mutiple on check works", {
  db <- letters_n(type = "database")
  expect_error(report_n(db, on = c(clnt_id, diagx)), "variable name")
})

test_that("on in an external vector with injection works", {
  db <- letters_n(type = "database")
  on_var <- "clnt_id"
  expect_type(report_n(db, on = !!on_var), "integer")
})
