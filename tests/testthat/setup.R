op <- options(
  healthdb.verbose = FALSE,
  healthdb.force_proceed = TRUE
)

withr::defer(options(op), teardown_env())

# run the whole suite against a client-server database instead of the default
# local SQLite by setting HEALTHDB_TEST_BACKEND
# (see .github/workflows/R-CMD-check.yaml):
# - "postgres": connect with 'RPostgres' using the standard PG* environment variables
# - "sqlserver": connect with 'odbc' using the connection string in HEALTHDB_TEST_ODBC
backend <- Sys.getenv("HEALTHDB_TEST_BACKEND")
if (nzchar(backend)) {
  test_con <- switch(backend,
    postgres = DBI::dbConnect(RPostgres::Postgres()),
    sqlserver = DBI::dbConnect(odbc::odbc(), .connection_string = Sys.getenv("HEALTHDB_TEST_ODBC")),
    stop("Unknown HEALTHDB_TEST_BACKEND: ", backend)
  )
  op_backend <- options(healthdb.test_con = test_con)
  withr::defer(
    {
      options(op_backend)
      DBI::dbDisconnect(test_con)
    },
    teardown_env()
  )
}
