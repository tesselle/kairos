.onLoad <- function(libname, pkgname){
  op <- options()
  op.fasti <- list(
    fasti.progress = TRUE
  )
  toset <- !(names(op.fasti) %in% names(op))
  if(any(toset)) options(op.fasti[toset])

  invisible()
}
