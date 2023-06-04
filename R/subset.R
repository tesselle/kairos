# SUBSET
#' @include AllGenerics.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,MeanDate-method
setMethod(
  f = "[",
  signature = c(x = "MeanDate"),
  function(x, i, j, ..., drop = FALSE) {
    z <- methods::callNextMethod() # Method for `TimeSeries`

    if (is.null(dim(z)) || isTRUE(drop)) return(z)

    dates <- x@dates
    if (!missing(j)) {
      dates <- dates[j]
    }
    methods::initialize(x, z, dates = dates)
  }
)

## [[ --------------------------------------------------------------------------
#' Extract Parts of an Object
#'
#' @inheritParams [[
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extract_slot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(i, choices = methods::slotNames(class_name),
                 several.ok = FALSE)
  data <- methods::slot(x, i)
  data
}

#' @export
#' @describeIn IncrementTest-class Extracts information from a slot selected
#'  by subscript i. i is a length-one character vector. Returns the
#'  corresponding slot values.
#' @aliases [[,IncrementTest,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "IncrementTest", i = "ANY", j = "missing"),
  definition = extract_slot
)

#' @export
#' @describeIn PermutationOrder-class Extracts information from a slot selected
#'  by subscript i. i is a length-one character vector. Returns the
#'  corresponding slot values.
#' @aliases [[,PermutationOrder,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PermutationOrder", i = "ANY", j = "missing"),
  definition = extract_slot
)
