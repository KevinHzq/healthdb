test_that("age restriction with birth_date works", {
  df <- letters_n(type = "data.frame")
  df$birth_dt <- as.Date("1990-01-01")

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    birth_date = birth_dt,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  # verify all ages are within range
  ages <- as.numeric(difftime(output_df$dates, output_df$birth_dt, units = "days")) / 365.25
  expect_true(all(ages >= 18 & ages <= 65))
})

test_that("age restriction with age column works", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    age = age,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("one-sided age bounds work (lower bound only)", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    age = age,
    age_range = c(18, NA),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18))
})

test_that("one-sided age bounds work (upper bound only)", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    age = age,
    age_range = c(NA, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age <= 65))
})

test_that("age restriction works with database backend", {
  skip_on_cran()
  # Use age column directly to avoid SQL translation issues with dates
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)
  db <- memdb_tbl(df)

  output_df <- define_case_with_age(db,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    age = age,
    age_range = c(18, 65),
    mode = "filter",
    force_collect = TRUE
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("age restriction + exclude works", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)
  excl_vals <- c("a", "b")

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    excl_vals = excl_vals,
    age = age,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("age restriction + restrict_n works", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    n_per_clnt = 2,
    age = age,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("age restriction + restrict_date works", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    n_per_clnt = 2,
    apart = 30,
    within = 365,
    uid = uid,
    age = age,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("age restriction + all features works", {
  df <- letters_n(type = "data.frame")
  df$birth_dt <- as.Date("1990-01-01")
  excl_vals <- c("a", "b")

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    excl_vals = excl_vals,
    n_per_clnt = 2,
    apart = 30,
    within = 365,
    uid = uid,
    birth_date = birth_dt,
    age_range = c(18, 65),
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
  ages <- abs(as.numeric(difftime(output_df$dates, output_df$birth_dt, units = "days"))) / 365.25
  expect_true(all(ages >= 18 & ages <= 65))
})

test_that("verbose output includes age restriction step", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  expect_message(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      age = age,
      age_range = c(18, 65),
      mode = "filter",
      verbose = TRUE
    ),
    "Age restriction"
  )
})

test_that("error if age_range is not length 2", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  expect_error(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      age = age,
      age_range = c(18),
      mode = "filter"
    ),
    "must be a length 2 vector"
  )
})

test_that("error if age_range has all NA values", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  expect_error(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      age = age,
      age_range = c(NA, NA),
      mode = "filter"
    ),
    "must have at least one non-NA value"
  )
})

test_that("error if neither birth_date nor age supplied", {
  df <- letters_n(type = "data.frame")

  expect_error(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      age_range = c(18, 65),
      mode = "filter"
    ),
    "Either `birth_date` or `age` must be supplied"
  )
})

test_that("error if both birth_date and age supplied", {
  df <- letters_n(type = "data.frame")
  df$birth_dt <- as.Date("1990-01-01")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  expect_error(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      birth_date = birth_dt,
      age = age,
      age_range = c(18, 65),
      mode = "filter"
    ),
    "Only one of `birth_date` or `age` should be supplied"
  )
})

test_that("error if date_var not supplied with birth_date", {
  df <- letters_n(type = "data.frame")
  df$birth_dt <- as.Date("1990-01-01")

  expect_error(
    define_case_with_age(df,
      starts_with("diagx"), "in", letters,
      clnt_id = clnt_id,
      birth_date = birth_dt,
      age_range = c(18, 65),
      mode = "filter"
    ),
    "`date_var` must be supplied to calculate age"
  )
})

test_that("flag mode works with age restriction", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    age = age,
    age_range = c(18, 65),
    mode = "flag"
  )

  expect_s3_class(output_df, "data.frame")
  # age restriction always filters regardless of mode
  # so output may have fewer rows than input
  expect_true(all(output_df$age >= 18 & output_df$age <= 65))
})

test_that("keep first/last works with age restriction", {
  df <- letters_n(type = "data.frame", id = 1:10)
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  df_all <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    uid = uid,
    age = age,
    age_range = c(18, 65),
    mode = "filter",
    keep = "all"
  )

  df_first <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    uid = uid,
    age = age,
    age_range = c(18, 65),
    mode = "filter",
    keep = "first"
  )

  df_last <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    uid = uid,
    age = age,
    age_range = c(18, 65),
    mode = "filter",
    keep = "last"
  )

  expect_true(nrow(df_all) >= nrow(df_first))
  expect_true(nrow(df_all) >= nrow(df_last))
  expect_s3_class(df_first, "data.frame")
  expect_s3_class(df_last, "data.frame")
})

test_that("age calculation is correct with different birth dates", {
  df <- data.frame(
    clnt_id = c(1, 1, 2, 2),
    dates = as.Date(c("2020-01-01", "2020-06-01", "2020-01-01", "2020-06-01")),
    birth_dt = as.Date(c("2000-01-01", "2000-01-01", "1970-01-01", "1970-01-01")),
    diagx = c("a", "b", "c", "d"),
    uid = 1:4
  )

  # age on 2020-01-01: person 1 is 20, person 2 is 50
  # age on 2020-06-01: person 1 is ~20.4, person 2 is ~50.4
  output_df <- define_case_with_age(df,
    diagx, "in", letters,
    clnt_id = clnt_id,
    date_var = dates,
    birth_date = birth_dt,
    age_range = c(21, 51),
    mode = "filter"
  )

  # should only keep records where age is between 21 and 51
  # person 1 at 2020-01-01: age 20 (excluded)
  # person 1 at 2020-06-01: age 20.4 (excluded)
  # person 2 at 2020-01-01: age 50 (included)
  # person 2 at 2020-06-01: age 50.4 (included)
  expect_equal(nrow(output_df), 2)
  expect_equal(unique(output_df$clnt_id), 2)
})

test_that("without age_range, function works as normal define_case", {
  df <- letters_n(type = "data.frame")
  df$age <- sample(10:80, nrow(df), replace = TRUE)

  # should work without age_range
  output_df <- define_case_with_age(df,
    starts_with("diagx"), "in", letters,
    clnt_id = clnt_id,
    mode = "filter"
  )

  expect_s3_class(output_df, "data.frame")
})
