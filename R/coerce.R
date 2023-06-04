# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame MeanDate
as.data.frame.MeanDate <- function(x, ..., calendar = getOption("kairos.calendar")) {
  methods::callNextMethod(x, ..., calendar = calendar)
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,MeanDate-method
setMethod("as.data.frame", "MeanDate", as.data.frame.MeanDate)

#' @export
#' @method as.data.frame AoristicSum
as.data.frame.AoristicSum <- function(x, ..., calendar = getOption("kairos.calendar")) {
  methods::callNextMethod(x, ..., calendar = calendar)
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,AoristicSum-method
setMethod("as.data.frame", "AoristicSum", as.data.frame.AoristicSum)

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
