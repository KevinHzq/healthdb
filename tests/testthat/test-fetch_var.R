test_that("str_split works", {
  expect_equal(stringr::str_split_fixed("sex", "\\|", n = 2)[, 2], "")
  expect_equal(stringr::str_split_1("sex", "\\+"), "sex")
  expect_equal(stringr::str_split_1("sex+age", "\\+"), c("sex", "age"))
})

test_that("basic use works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1, keys = clnt_id,
             linkage = list(df2 ~ sex))
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("multiple keys works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans),
                       linkage = list(df2 ~ sex))
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("subset of keys works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE),
                    diagx = sample(letters, nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans, diagx),
                       linkage = list(df2 ~ sex|clnt_id))
  expect_in(out_df$sex, c(df2$sex, NA))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans, diagx),
                       linkage = list(df2 ~ sex|clnt_id + ans))
  expect_in(out_df$sex, c(df2$sex, NA))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans, diagx),
                       linkage = list(df2 ~ sex|clnt_id + ans + diagx))
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("multiple sources works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE))
  db3 <- dbplyr::memdb_frame(clnt_id = 1:nrow(df1), age = sample(0:100, nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans),
                       linkage = list(df2 ~ sex,
                                      db3 ~ age),
                       copy = TRUE)
  expect_in(out_df$sex, c(df2$sex, NA))
  expect_in(out_df$age, c(dplyr::pull(db3, age), NA))
})

test_that("multiple keys works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1, keys = c(clnt_id, ans),
                       linkage = list(df2 ~ sex))
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("n of keys check works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
                    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE))
  expect_error(fetch_var(df1, keys = clnt_id,
                          linkage = list(df2 ~ sex|clnt_id + ans)),
               "length of variables")
  # also test var not in keys
  expect_error(fetch_var(df1, keys = clnt_id,
                          linkage = list(df2 ~ sex|ans)),
               "subset of keys")
})
