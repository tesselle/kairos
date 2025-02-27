.onLoad <- function(libname, pkgname){
  op <- options()
  op.kairos <- list(
    kairos.verbose = interactive(),
    kairos.progress = interactive()
  )
  toset <- !(names(op.kairos) %in% names(op))
  if(any(toset)) options(op.kairos[toset])

  invisible()
}
