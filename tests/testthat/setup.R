op <- options(healthdb.verbose = FALSE,
              healthdb.force_proceed = TRUE)

withr::defer(options(op), teardown_env())
