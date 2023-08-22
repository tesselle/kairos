.onLoad <- function(libname, pkgname){
  op <- options()
  op.kairos <- list(
    kairos.progress = TRUE,
    kairos.calendar = aion::CE()
  )
  toset <- !(names(op.kairos) %in% names(op))
  if(any(toset)) options(op.kairos[toset])

  invisible()
}
