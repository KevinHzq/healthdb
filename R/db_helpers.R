# one in-memory SQLite database shared for the whole session, so that tables
# copied by separate memdb_tbl() calls can be joined with each other.
# 'dbplyr' offers the same thing, but under a name that has changed across
# versions (src_memdb() was deprecated in 2.6.0 in favour of memdb(), which does
# not exist before it), so the connection is made directly instead.
memdb_cache <- new.env(parent = emptyenv())

memdb_con <- function() {
  con <- memdb_cache$con
  # short-circuits before touching DBI when nothing has been cached yet
  if (!is.null(con) && DBI::dbIsValid(con)) {
    return(con)
  }
  rlang::check_installed(c("DBI", "RSQLite"), "to make an in-memory test database.")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  memdb_cache$con <- con
  con
}

# convert df to a temporary table in the testing backend: a connection supplied
# via options(healthdb.test_con = con), e.g., PostgreSQL in CI, or the default
# local SQLite memdb
memdb_tbl <- function(df) {
  tab_nm <- rawToChar(as.raw(sample(c(65:90, 97:122, 48:57), 20, replace = T)))
  con <- getOption("healthdb.test_con")
  if (is.null(con)) {
    con <- memdb_con()
  }
  dplyr::copy_to(con, df, tab_nm, temporary = TRUE, overwrite = TRUE)
}

# clean up all the grouping and ordering in tbl_sql object
clean_db <- function(db) {
  db %>%
    dplyr::ungroup() %>%
    dbplyr::window_order()
}

# confirm a potentially slow operation with the user;
# abort in non-interactive sessions instead of silently proceeding
# (readline() returns "" when not interactive, which is not "n")
ask_proceed <- function(why, hint = NULL) {
  if (!rlang::is_interactive()) {
    stop(
      "\n", why,
      " This needs confirmation, but the session is not interactive.",
      " Use force_proceed = TRUE (or options(healthdb.force_proceed = TRUE)) to proceed.",
      if (!is.null(hint)) paste0("\n", hint),
      call. = FALSE
    )
  }

  proceed <- readline(prompt = paste(why, "Proceed? [y/n]"))

  if (!tolower(trimws(proceed)) %in% c("y", "yes")) {
    stop(
      "\nCancelled by user.",
      if (!is.null(hint)) paste0("\n", hint),
      call. = FALSE
    )
  }

  invisible()
}

# test db connection; can be skipped with options(healthdb.check_con = FALSE)
# to avoid one round trip to the server per verb in a long pipeline
check_con <- function(data) {
  if (!isTRUE(getOption("healthdb.check_con", TRUE))) {
    return(invisible())
  }

  con <- dbplyr::remote_con(data)
  tryCatch(dbplyr::db_collect(con, sql = "SELECT 1"),
    error = function(err) {
      rlang::abort("\nDatabase connection failed. Troubleshooting steps: 1. Make sure the connection is not closed; 2. Verify network connectivity to the database server; 3. Check database credentials are correct; 4. Confirm the database server is running; 5. Ensure firewall rules allow the connection", parent = err)
    }
  )
  return(invisible())
}
