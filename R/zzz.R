#set option for verbose
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.odcfun <- list(
    odcfun.verbose = TRUE,
    odcfun.force_proceed = FALSE
  )
  toset <- !(names(op.odcfun) %in% names(op))
  if (any(toset)) options(op.odcfun[toset])

  invisible()
}

#clean up
.onUnload <- function(libpath) {
  op <- options()
  op.odcfun <- list(
    odcfun.verbose = NULL,
    odcfun.force_proceed = NULL
  )
  toremove <- names(op.odcfun) %in% names(op)
  if (any(toremove)) options(op.odcfun[toremove])

  invisible()
}
