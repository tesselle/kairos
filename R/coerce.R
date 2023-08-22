# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame MeanDate
as.data.frame.MeanDate <- function(x, ..., calendar = getOption("kairos.calendar")) {
  data.frame(
    sample = rownames(x) %||% paste0("S", seq_len(nrow(x))),
    time = aion::time(x, calendar = calendar)
  )
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

#' @export
#' @method as.data.frame RateOfChange
as.data.frame.RateOfChange <- function(x, ..., calendar = getOption("kairos.calendar")) {
  methods::callNextMethod(x, ..., calendar = calendar)
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,RateOfChange-method
setMethod("as.data.frame", "RateOfChange", as.data.frame.RateOfChange)

#' @method as.data.frame IncrementTest
#' @export
as.data.frame.IncrementTest <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    t = x@statistic,
    p.value = x@p_value,
    row.names = colnames(x),
    ...
  )
}

#' @export
#' @rdname data.frame
#' @aliases as.data.frame,IncrementTest-method
setMethod("as.data.frame", "IncrementTest", as.data.frame.IncrementTest)
