# convert df to memdb table
memdb_tbl <- function(df) {
  tab_nm <- rawToChar(as.raw(sample(c(65:90, 97:122, 48:57), 20, replace = T)))
  dplyr::copy_to(dbplyr::src_memdb(), df, tab_nm, temporary = TRUE, overwrite = TRUE)
  dplyr::tbl(dbplyr::src_memdb(), tab_nm)
}

# clean up all the grouping and ordering in tbl_sql object
clean_db <- function(db) {
  db %>%
    dplyr::ungroup() %>%
    dbplyr::window_order()
}
