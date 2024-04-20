test_that("answer is correct for icd9 df", {
  input <- test_comorbidity(icd10 = FALSE)
  answer <- input[["answer"]]
  df <- input[["data"]]
  result <- compute_comorbidity(df,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  )
  result <- dplyr::arrange(result, uid) %>%
    dplyr::select(chf:total_eci)
  expect_equal(answer, result)
})

test_that("answer is correct for icd10 df", {
  input <- test_comorbidity()
  answer <- input[["answer"]]
  df <- input[["data"]]
  result <- compute_comorbidity(df,
    vars = starts_with("diagx"),
    icd_ver = "ICD-10", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  )
  result <- dplyr::arrange(result, uid) %>%
    dplyr::select(chf:total_eci)
  expect_equal(answer, result)
})

test_that("answer is correct for icd9 db", {
  input <- test_comorbidity(icd10 = FALSE)
  answer <- input[["answer"]]
  df <- input[["data"]]
  db <- memdb_tbl(df)
  result <- compute_comorbidity(db,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  ) %>%
    dplyr::collect()
  result <- dplyr::arrange(result, uid) %>%
    dplyr::select(chf:total_eci)
  expect_equal(answer, result)
})

test_that("answer of icd9 3digit is different from 5digits df", {
  input <- test_comorbidity(icd10 = FALSE)
  # answer <- input[["answer"]]
  df <- input[["data"]]
  result3 <- compute_comorbidity(df,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-3digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  )
  result5 <- compute_comorbidity(df,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  )
  expect_false(identical(result3, result5))
})

test_that("edge case - one vars column works", {
  input <- test_comorbidity(icd10 = FALSE)
  answer <- input[["answer"]]
  df <- input[["data"]]
  db <- memdb_tbl(df)
  result <- compute_comorbidity(db,
    vars = diagx_1,
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  ) %>%
    dplyr::collect()
  expect_equal(nrow(answer), nrow(result))
})

test_that("exclue works db", {
  input <- test_comorbidity(icd10 = FALSE)
  answer <- input[["answer"]]
  df <- input[["data"]]
  db <- memdb_tbl(df)

  # get the sum of excl
  excl_cols <- sample(colnames(answer), 3)
  answer <- answer %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      excl_eci = sum(dplyr::c_across(dplyr::any_of(excl_cols))),
      total_eci_excl = total_eci - excl_eci
    )

  result <- compute_comorbidity(db,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = excl_cols
  ) %>%
    dplyr::collect()
  result <- dplyr::arrange(result, uid) %>%
    dplyr::select(chf:total_eci)

  expect_equal(answer$total_eci_excl, result$total_eci)
})

test_that("answer is correct for icd9 db", {
  input <- test_comorbidity(icd10 = FALSE)
  answer <- input[["answer"]]
  df <- input[["data"]]
  db <- memdb_tbl(df)
  result <- compute_comorbidity(db,
    vars = starts_with("diagx"),
    icd_ver = "ICD-9-CM-5digits", clnt_id = clnt_id,
    uid = uid, sum_by = "clnt", excl = NULL
  ) %>%
    dplyr::collect()
  answer$clnt_id <- df$clnt_id
  answer <- answer %>%
    dplyr::group_by(clnt_id) %>%
    dplyr::summarise(dplyr::across(c(-total_eci), max)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_eci = sum(dplyr::c_across(c(chf:dep)))) %>%
    dplyr::ungroup()
  expect_equal(answer, result)
})
