# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x@data),
    stringsAsFactors = FALSE
  )
}

#' @method as.data.frame DateMCD
#' @export
as.data.frame.DateMCD <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    date = x@mcd_values,
    error = x@mcd_errors,
    row.names = rownames(x@data),
    stringsAsFactors = FALSE
  )
}
