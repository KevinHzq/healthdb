test_that("basic use works", {
  df1 <- make_test_dat()
  df2 <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2)
  out_df <- bind_sources(list(df1, df2), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_id", "clnt_id", "dx_date", "icd9", "icd10"))
  expect_error(bind_sources(list(df1, df2, df1), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx")), "number of sources")
})

test_that("stored list object input works", {
  df1 <- make_test_dat()
  df2 <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2)
  df_list <- list(df1, df2)
  out_df <- bind_sources(df_list, clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_id", "clnt_id", "dx_date", "icd9", "icd10"))
  expect_error(bind_sources(list(df1, df2, df1), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx")), "number of sources")
})

test_that("stored list object input works", {
  df1 <- make_test_dat(type = "database")
  df2 <- make_test_dat(vals_kept = paste0("F1", 1:9), seed = 2, type = "database")
  df_list <- list(df1, df2)
  out_df <- bind_sources(df_list, clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx"))
  expect_s3_class(out_df, "data.frame")
  expect_setequal(names(out_df), c("src_id", "clnt_id", "dx_date", "icd9", "icd10"))
  expect_error(bind_sources(list(df1, df2, df1), clnt_id = "clnt_id", dx_date = "dates", icd9 = "diagx", icd10 = c(NA, "diagx")), "number of sources")
})
