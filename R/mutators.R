# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname series
#' @aliases time,EventDate-method
setMethod(
  f = "time",
  signature = "EventDate",
  definition = function(x, calendar = NULL) {
    z <- x@dates
    if (is.null(calendar)) return(z)
    aion::as_year(z, calendar = calendar, decimal = TRUE)
  }
)

#' @export
#' @rdname series
#' @aliases span,AoristicSum-method
setMethod(
  f = "span",
  signature = "AoristicSum",
  definition = function(x, calendar = NULL) {
    z <- x@span
    if (is.null(calendar)) return(unclass(z))
    aion::as_year(z, calendar = calendar, shift = FALSE)
  }
)

#' @export
#' @rdname mutators
#' @aliases weights,AoristicSum-method
setMethod("weights", "AoristicSum", function(object, ...) object@p)

#' @export
#' @rdname mutators
#' @aliases weights,CountApportion-method
setMethod("weights", "CountApportion", function(object, ...) object@p)

# Setters ======================================================================
