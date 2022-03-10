# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame MeanDate
#' @export
as.data.frame.MeanDate <- function(x, ..., stringsAsFactors = FALSE) {
  data.frame(
    names = names(x),
    dates = as.numeric(x),
    stringsAsFactors = stringsAsFactors
  )
}

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, ..., stringsAsFactors = FALSE) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x@counts),
    stringsAsFactors = stringsAsFactors
  )
}
