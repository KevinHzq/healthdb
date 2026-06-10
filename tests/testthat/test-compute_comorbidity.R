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

test_that("answer is correct for icd10 db", {
  input <- test_comorbidity()
  answer <- input[["answer"]]
  df <- input[["data"]]
  db <- memdb_tbl(df)
  result <- compute_comorbidity(db,
    vars = starts_with("diagx"),
    icd_ver = "ICD-10", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  ) %>%
    dplyr::collect()
  result <- dplyr::arrange(result, uid) %>%
    dplyr::select(chf:total_eci)
  expect_equal(answer, result)
})

test_that("icd10 codes longer than the listed ones are matched by prefix", {
  df <- data.frame(
    uid = 1:4, clnt_id = 1:4,
    # I5010 -> I50 (chf, 3-char category)
    # E1152 -> E115 (diab_c, 4-char subcategory)
    # F3290 -> F32 (dep, 3-char category)
    # X999 matches nothing
    diagx_1 = c("I5010", "E1152", "F3290", "X999"),
    diagx_2 = NA_character_
  )
  for (dat in list(df, memdb_tbl(df))) {
    result <- compute_comorbidity(dat,
      vars = starts_with("diagx"),
      icd_ver = "ICD-10", clnt_id = clnt_id,
      uid = uid, sum_by = "row", excl = NULL
    ) %>%
      dplyr::collect() %>%
      dplyr::arrange(uid)
    expect_equal(result$chf, c(1, 0, 0, 0))
    expect_equal(result$diab_c, c(0, 1, 0, 0))
    expect_equal(result$dep, c(0, 0, 1, 0))
    expect_equal(result$total_eci, c(1, 1, 1, 0))
  }
})

test_that("icd10 codes in overlapping categories count in all of them", {
  # per Quan et al., I11.0 is in both chf and hptn_c (I11.x)
  df <- data.frame(
    uid = 1L, clnt_id = 1L,
    diagx_1 = "I110",
    diagx_2 = NA_character_
  )
  result <- compute_comorbidity(df,
    vars = starts_with("diagx"),
    icd_ver = "ICD-10", clnt_id = clnt_id,
    uid = uid, sum_by = "row", excl = NULL
  )
  expect_equal(result$chf, 1)
  expect_equal(result$hptn_c, 1)
  expect_equal(result$total_eci, 2)
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
  # total_eci is not a category and must not be drawn
  excl_cols <- sample(setdiff(colnames(answer), "total_eci"), 3)
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

test_that("every code in elix_codes maps to its own category", {
  for (ver in unique(elix_codes$icd_ver)) {
    lu <- subset(elix_codes, icd_ver == ver)
    df <- data.frame(
      uid = seq_len(nrow(lu)), clnt_id = seq_len(nrow(lu)),
      diagx_1 = lu$code, diagx_2 = NA_character_
    )
    result <- compute_comorbidity(df,
      vars = starts_with("diagx"), icd_ver = ver,
      clnt_id = clnt_id, uid = uid
    ) %>% dplyr::arrange(uid)
    for (cat in unique(lu$category)) {
      expect_true(all(result[[cat]][lu$category == cat] == 1), label = paste(ver, cat))
    }
  }
})

test_that("prefix codes in elix_codes match their subdivisions", {
  lu <- subset(elix_codes, !is.na(match_len))
  for (ver in unique(lu$icd_ver)) {
    lv <- subset(lu, icd_ver == ver)
    df <- data.frame(
      uid = seq_len(nrow(lv)), clnt_id = seq_len(nrow(lv)),
      diagx_1 = paste0(lv$code, "1"), diagx_2 = NA_character_
    )
    result <- compute_comorbidity(df,
      vars = starts_with("diagx"), icd_ver = ver,
      clnt_id = clnt_id, uid = uid
    ) %>% dplyr::arrange(uid)
    for (cat in unique(lv$category)) {
      expect_true(all(result[[cat]][lv$category == cat] == 1), label = paste(ver, cat, "prefix"))
    }
  }
})
