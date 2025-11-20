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

# test db connection
check_con <- function(data) {
  con <- dbplyr::remote_con(data)
  tryCatch(dbplyr::db_collect(con, sql = "SELECT 1"),
    error = function(err) {
      rlang::abort("\nDatabase connection failed. Troubleshooting steps: 1. Make sure the connection is not closed; 2. Verify network connectivity to the database server; 3. Check database credentials are correct; 4. Confirm the database server is running; 5. Ensure firewall rules allow the connection", parent = err)
    }
  )
  return(invisible())
}
