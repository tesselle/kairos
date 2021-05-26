.onLoad <- function(libname, pkgname){
  op <- options()
  op.kairos <- list(
    kairos.progress = TRUE
  )
  toset <- !(names(op.kairos) %in% names(op))
  if(any(toset)) options(op.kairos[toset])

  invisible()
}
