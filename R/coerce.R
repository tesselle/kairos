# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame AoristicSum
#' @export
as.data.frame.AoristicSum <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  x <- t(x@sum)
  as.data.frame(x, stringsAsFactors = stringsAsFactors)
}

#' @method as.data.frame DateMCD
#' @export
as.data.frame.DateMCD <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    samples = x@samples,
    dates = x@dates_mcd,
    row.names = rownames(x),
    stringsAsFactors = stringsAsFactors
  )
}

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x@counts),
    stringsAsFactors = stringsAsFactors
  )
}
