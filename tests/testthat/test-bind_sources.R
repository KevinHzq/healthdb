test_that("basic use works", {
  df1 <- make_test_dat()
  df2 <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2)
  out_df <- bind_sources(list(df1, df2), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_No", "clnt_id", "dx_date", "icd9", "icd10"))
  # also test for n_src != n_var error
  expect_error(bind_sources(list(df1, df2, df1), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx")), "number of sources")
})

test_that("stored list object input works", {
  df1 <- make_test_dat()
  df2 <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2)
  df_list <- list(df1, df2)
  out_df <- bind_sources(df_list, clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_No", "clnt_id", "dx_date", "icd9", "icd10"))
})

test_that("different input type works", {
  db <- make_test_dat(type = "database")
  df <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2) %>%
    dplyr::mutate(dates = as.numeric(dates))
  # 1. mixed
  dat_list <- list(db, df)
  out_df <- bind_sources(dat_list, clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_No", "clnt_id", "dx_date", "icd9", "icd10"))
  # 2. all remote
  dat_list <- list(db, db)
  out_db <- bind_sources(dat_list, clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_db, "tbl_sql")
  expect_setequal(colnames(out_db), c("clnt_id", "dx_date", "icd9", "icd10"))
  # 3. all local have been tested before
})
