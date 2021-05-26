# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# To data.frame ================================================================
#' @method as.data.frame AoristicSum
#' @export
as.data.frame.AoristicSum <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  grp <- unique(x@groups)
  data.frame(
    dates = x@dates,
    sum = x@sum,
    groups = rep(grp, each = length(x@dates) / length(grp)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' @method as.data.frame DateMCD
#' @export
as.data.frame.DateMCD <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {
  data.frame(
    date = x@mcd,
    row.names = rownames(x@counts),
    stringsAsFactors = FALSE
  )
}

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
