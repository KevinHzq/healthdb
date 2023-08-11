#set option for verbose
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.odcfun <- list(
    odcfun.verbose = TRUE
  )
  toset <- !(names(op.odcfun) %in% names(op))
  if (any(toset)) options(op.odcfun[toset])

  invisible()
}

#clean up
.onUnload <- function(libpath) {
  options(odcfun.verbose = NULL)
  invisible()
}
