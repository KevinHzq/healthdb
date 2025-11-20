# set option for verbose
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.healthdb <- list(
    healthdb.verbose = TRUE,
    healthdb.force_proceed = FALSE
  )
  toset <- !(names(op.healthdb) %in% names(op))
  if (any(toset)) options(op.healthdb[toset])

  invisible()
}

# clean up
.onUnload <- function(libpath) {
  op <- options()
  op.healthdb <- list(
    healthdb.verbose = NULL,
    healthdb.force_proceed = NULL
  )
  toremove <- names(op.healthdb) %in% names(op)
  if (any(toremove)) options(op.healthdb[toremove])

  invisible()
}
