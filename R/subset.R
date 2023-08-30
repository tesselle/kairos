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
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    time <- x@.Time
    dates <- x@dates

    z <- z[i, j, k, drop = drop]
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      time <- time[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, dimnames(x)[2L])
      dates <- dates[j]
    }

    if (isTRUE(drop)) return(z)
    methods::initialize(x, z, .Time = time, dates = dates)
  }
)

#' @export
#' @rdname subset
#' @aliases [,IncrementTest-method
setMethod(
  f = "[",
  signature = c(x = "IncrementTest"),
  function(x, i, j, k, drop = FALSE) {
    z <- x@.Data
    time <- x@.Time
    statistic <- x@statistic
    p_value <- x@p_value

    z <- z[i, j, k, drop = drop]
    if (!missing(i)) {
      if (is.character(i)) i <- match(i, dimnames(x)[1L])
      time <- time[i]
    }
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, dimnames(x)[2L])
      statistic <- statistic[j]
      p_value <- p_value[j]
    }

    if (isTRUE(drop)) return(z)
    methods::initialize(x, z, .Time = time, statistic = statistic, p_value = p_value)
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
#' @rdname subset
#' @aliases [[,PermutationOrder,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PermutationOrder", i = "ANY", j = "missing"),
  definition = extract_slot
)
