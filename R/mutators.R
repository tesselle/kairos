# MUTATORS
#' @include AllClasses.R
NULL

# Getters ======================================================================
#' @export
#' @rdname series
#' @aliases get_dates,EventDate-method
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
#' @rdname mutators
#' @aliases weights,AoristicSum-method
setMethod("weights", "AoristicSum", function(object, ...) object@weights)

#' @export
#' @rdname mutators
#' @aliases weights,CountApportion-method
setMethod("weights", "CountApportion", function(object, ...) object@p)

# Setters ======================================================================
