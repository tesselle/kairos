.onLoad <- function(libname, pkgname){
  op <- options()
  op.khronos <- list(
    khronos.progress = TRUE
  )
  toset <- !(names(op.khronos) %in% names(op))
  if(any(toset)) options(op.khronos[toset])

  invisible()
}
