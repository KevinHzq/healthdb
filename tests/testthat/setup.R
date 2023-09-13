op <- options(odcfun.verbose = FALSE,
              odcfun.force_proceed = TRUE)

withr::defer(options(op), teardown_env())
