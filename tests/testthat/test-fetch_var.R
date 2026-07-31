test_that("str_split works", {
  expect_equal(stringr::str_split_fixed("sex", "\\|", n = 2)[, 2], "")
  expect_equal(stringr::str_split_1("sex", "\\+"), "sex")
  expect_equal(stringr::str_split_1("sex+age", "\\+"), c("sex", "age"))
})

test_that("basic use works", {
  df1 <- letters_n()
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE))
  out_df <- fetch_var(df1,
    keys = clnt_id,
    linkage = list(df2 ~ sex)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("multiple keys works", {
  df1 <- letters_n()
  df2 <- data.frame(
    clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  )
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans),
    linkage = list(df2 ~ sex)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("subset of keys works", {
  df1 <- letters_n()
  df2 <- data.frame(
    clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE),
    diagx = sample(letters, nrow(df1), replace = TRUE)
  )
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans, diagx),
    linkage = list(df2 ~ sex | clnt_id)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans, diagx),
    linkage = list(df2 ~ sex | clnt_id + ans)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans, diagx),
    linkage = list(df2 ~ sex | clnt_id + ans + diagx)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("multiple sources works", {
  df1 <- letters_n()
  df2 <- data.frame(
    clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  )
  db3 <- memdb_tbl(dplyr::tibble(
    clnt_id = 1:nrow(df1), age = sample(0:100, nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  ))
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans),
    linkage = list(
      df2 ~ sex,
      db3 ~ age
    ),
    copy = TRUE
  )
  expect_in(out_df$sex, c(df2$sex, NA))
  expect_in(out_df$age, c(dplyr::pull(db3, age), NA))
})

test_that("multiple keys works", {
  df1 <- letters_n()
  df2 <- data.frame(
    clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  )
  out_df <- fetch_var(df1,
    keys = c(clnt_id, ans),
    linkage = list(df2 ~ sex)
  )
  expect_in(out_df$sex, c(df2$sex, NA))
})

test_that("n of keys check works", {
  df1 <- letters_n()
  df2 <- data.frame(
    clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  )
  expect_error(
    fetch_var(df1,
      keys = clnt_id,
      linkage = list(df2 ~ sex | clnt_id + ans)
    ),
    "length of variables"
  )
  # also test var not in keys
  expect_error(
    fetch_var(df1,
      keys = clnt_id,
      linkage = list(df2 ~ sex | ans)
    ),
    "subset of keys"
  )
})

test_that("not one to one warning works", {
  df1 <- letters_n(id = 1:5)
  df2 <- data.frame(clnt_id = c(1:5, 1:5), sex = c(rep("F", 5), rep("M", 5)))
  expect_error(fetch_var(df1,
    keys = clnt_id,
    linkage = list(df2 ~ sex)
  ), "not one to one") %>%
    expect_warning()
})

test_that("database x works", {
  db1 <- letters_n(type = "database")
  df1 <- dplyr::collect(db1)
  df2 <- data.frame(clnt_id = 1:nrow(df1), sex = sample(c("F", "M"), nrow(df1), replace = TRUE))
  db3 <- memdb_tbl(dplyr::tibble(
    clnt_id = 1:nrow(df1), age = sample(0:100, nrow(df1), replace = TRUE),
    ans = sample(c("all", "any", "noise"), nrow(df1), replace = TRUE)
  ))
  out_df <- fetch_var(db1,
    keys = c(clnt_id, ans),
    linkage = list(
      df2 ~ sex | clnt_id,
      db3 ~ age
    ),
    copy = TRUE
  ) %>%
    dplyr::collect()
  expect_in(out_df$sex, c(df2$sex, NA))
  expect_in(out_df$age, c(dplyr::pull(db3, age), NA))
})

test_that("result does not rely on left_join preserving row order", {
  # simulates join backends that do not preserve x's row order, e.g., duckdb
  # joins via duckplyr::methods_overwrite(), by shuffling the joined rows
  df1 <- data.frame(
    clnt_id = rep(1:3, each = 2), year = rep(2020:2021, 3),
    v = 1:6
  )
  df2 <- data.frame(clnt_id = 1:3, sex = c("F", "M", "F"))
  df3 <- data.frame(
    clnt_id = rep(1:3, each = 2), year = rep(2020:2021, 3),
    age = 21:26
  )
  real_left_join <- dplyr::left_join
  local_mocked_bindings(
    left_join = function(x, ...) {
      out <- real_left_join(x, ...)
      out[sample(nrow(out)), , drop = FALSE]
    },
    .package = "dplyr"
  )
  out_df <- fetch_var(df1,
    keys = c(clnt_id, year),
    linkage = list(
      df2 ~ sex | clnt_id,
      df3 ~ age
    )
  )
  expect_equal(out_df$sex, c("F", "F", "M", "M", "F", "F"))
  expect_equal(out_df$age, 21:26)
  expect_false(".fetch_var_row_id" %in% colnames(out_df))
})

test_that("fetched name colliding with an excluded key errors", {
  df1 <- data.frame(clnt_id = rep(1:3, each = 2), year = rep(2020:2021, 3), v = 1:6)
  # y carries a `year` column with a different meaning (e.g., birth year)
  df2 <- data.frame(clnt_id = 1:3, sex = c("F", "M", "F"), year = c(1999, 1998, 1997))
  expect_error(
    fetch_var(df1,
      keys = c(clnt_id, year),
      linkage = list(df2 ~ c(sex, year) | clnt_id)
    ),
    "keys not included"
  )
  # also triggered by greedy tidyselect
  expect_error(
    fetch_var(df1,
      keys = c(clnt_id, year),
      linkage = list(df2 ~ everything() | clnt_id)
    ),
    "keys not included"
  )
})

test_that("fetched name colliding with data's columns errors", {
  df1 <- data.frame(clnt_id = 1:3, v = 1:3)
  df2 <- data.frame(clnt_id = 1:3, v = c(9, 8, 7))
  expect_error(
    fetch_var(df1,
      keys = clnt_id,
      linkage = list(df2 ~ v)
    ),
    "already exist"
  )
})

test_that("same name fetched from multiple sources errors", {
  df1 <- data.frame(clnt_id = 1:3, year = 2020:2022)
  df2 <- data.frame(clnt_id = 1:3, sex = c("F", "M", "F"))
  df3 <- data.frame(year = 2020:2022, sex = c("M", "F", "M"))
  expect_error(
    fetch_var(df1,
      keys = c(clnt_id, year),
      linkage = list(
        df2 ~ sex | clnt_id,
        df3 ~ sex | year
      )
    ),
    "more than one source"
  )
})

test_that("non-formula element in linkage errors", {
  df1 <- data.frame(clnt_id = 1:3)
  df2 <- data.frame(clnt_id = 1:3, sex = c("F", "M", "F"))
  expect_error(
    fetch_var(df1, keys = clnt_id, linkage = list(df2 ~ sex, "oops")),
    "is_formula"
  )
})

test_that("duplicated key after '|' is tolerated", {
  df1 <- data.frame(clnt_id = 1:3, year = 2020:2022)
  df2 <- data.frame(clnt_id = 1:3, sex = c("F", "M", "F"))
  out_df <- fetch_var(df1,
    keys = c(clnt_id, year),
    linkage = list(df2 ~ sex | clnt_id + clnt_id)
  )
  expect_equal(out_df$sex, c("F", "M", "F"))
})
